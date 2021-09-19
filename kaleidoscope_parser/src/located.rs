use kaleidoscope_ast::XWrapper;
use nom::{
    error::{ErrorKind, ParseError},
    AsBytes, Compare, CompareResult, Err, FindSubstring, FindToken, IResult, InputIter,
    InputLength, InputTake, InputTakeAtPosition, Needed, Offset, ParseTo, Parser, Slice,
};
use std::{
    ops::{Range, RangeFrom, RangeFull, RangeTo},
    str::{CharIndices, Chars, FromStr},
};

pub trait Located {
    fn new(range: Range<usize>) -> Self;
    fn position(&self) -> &Range<usize>;
}

#[derive(Debug, PartialEq)]
pub struct Position {
    range: Range<usize>,
}

impl Located for Position {
    fn new(range: Range<usize>) -> Self {
        Position { range }
    }

    fn position(&self) -> &Range<usize> {
        &self.range
    }
}

pub trait LocatedInput {
    fn position(&self) -> usize;
}

pub fn position<I: LocatedInput, E: ParseError<I>>(i: I) -> IResult<I, usize, E> {
    let pos = i.position();
    Ok((i, pos))
}

pub fn locate<I, O, E, P, L>(mut parser: P) -> impl FnMut(I) -> IResult<I, XWrapper<O, L>, E>
where
    E: ParseError<I>,
    L: Located,
    P: Parser<I, O, E>,
    I: LocatedInput,
{
    move |i: I| {
        let start = i.position();
        let (suffix, token) = parser.parse(i)?;
        let end = suffix.position();
        Ok((suffix, XWrapper::new(token, Located::new(start..end))))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocatedSlice<'a> {
    start_ptr: usize,
    slice: &'a str,
}

impl<'a> LocatedSlice<'a> {
    pub fn new(slice: &'a str) -> Self {
        let start_ptr = slice.as_ptr() as usize;
        LocatedSlice { start_ptr, slice }
    }
    #[inline]
    fn with_slice<'b>(&self, slice: &'b str) -> LocatedSlice<'b> {
        LocatedSlice {
            start_ptr: self.start_ptr,
            slice,
        }
    }
    fn unsafe_split(&self, i: usize) -> (Self, Self) {
        unsafe {
            (
                self.with_slice(self.slice.get_unchecked(i..)),
                self.with_slice(self.slice.get_unchecked(..i)),
            )
        }
    }
}

impl<'a> LocatedInput for LocatedSlice<'a> {
    fn position(&self) -> usize {
        self.slice.as_ptr() as usize - self.start_ptr
    }
}

macro_rules! slice_range_impl {
    ($ty:ty) => {
        impl<'a> Slice<$ty> for LocatedSlice<'a> {
            fn slice(&self, range: $ty) -> Self {
                self.with_slice(self.slice.slice(range))
            }
        }
    };
}

slice_range_impl!(Range<usize>);
slice_range_impl!(RangeFrom<usize>);
slice_range_impl!(RangeTo<usize>);
slice_range_impl!(RangeFull);

// macro_rules! impl_InputTakeAtPosition {
//     ($method:ident) => {
//         fn $method<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
//         where
//             P: Fn(Self::Item) -> bool,
//         {
//             let (first, second) = self.slice.$method(predicate)?;
//             Ok((self.with_slice(first), self.with_slice(second)))
//         }
//     };
// }

impl<'a> InputTakeAtPosition for LocatedSlice<'a> {
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => Ok(self.unsafe_split(i)),
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }
    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => Ok(self.unsafe_split(i)),
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => Ok(self.unsafe_split(i)),
            // the end of slice is a char boundary
            None => Ok(self.unsafe_split(self.slice.len())),
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => Ok(self.unsafe_split(i)),
            None => {
                if self.slice.is_empty() {
                    Err(Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    // the end of slice is a char boundary
                    Ok(self.unsafe_split(self.slice.len()))
                }
            }
        }
    }
}

impl<'a> InputIter for LocatedSlice<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.slice.iter_indices()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.slice.iter_elements()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.slice.position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.slice.slice_index(count)
    }
}

impl<'a> InputLength for LocatedSlice<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.slice.input_len()
    }
}

impl<'a> InputTake for LocatedSlice<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        self.with_slice(self.slice.take(count))
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (suffix, prefix) = self.slice.take_split(count);
        (self.with_slice(suffix), self.with_slice(prefix))
    }
}

impl<'a> AsBytes for LocatedSlice<'a> {
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.slice.as_bytes()
    }
}

impl<'a, R: FromStr> ParseTo<R> for LocatedSlice<'a> {
    #[inline]
    fn parse_to(&self) -> Option<R> {
        self.slice.parse_to()
    }
}

impl<'a> Offset for LocatedSlice<'a> {
    #[inline]
    fn offset(&self, second: &Self) -> usize {
        self.slice.offset(second.slice)
    }
}

impl<'a, 'b> FindSubstring<&'b str> for LocatedSlice<'a> {
    #[inline]
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.slice.find_substring(substr)
    }
}
impl<'a, 'b> FindSubstring<LocatedSlice<'b>> for LocatedSlice<'a> {
    #[inline]
    fn find_substring(&self, substr: LocatedSlice<'b>) -> Option<usize> {
        self.find_substring(substr.slice)
    }
}

impl<'a, Token> FindToken<Token> for LocatedSlice<'a>
where
    &'a str: FindToken<Token>,
{
    fn find_token(&self, token: Token) -> bool {
        self.slice.find_token(token)
    }
}

impl<'a, B> Compare<B> for LocatedSlice<'a>
where
    &'a str: Compare<B>,
{
    fn compare(&self, t: B) -> CompareResult {
        self.slice.compare(t)
    }

    fn compare_no_case(&self, t: B) -> CompareResult {
        self.slice.compare_no_case(t)
    }
}

impl<'a> Into<String> for LocatedSlice<'a> {
    fn into(self) -> String {
        self.slice.into()
    }
}
