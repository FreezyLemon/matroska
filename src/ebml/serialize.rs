use std::{io::Write, mem::size_of};

use cookie_factory::{
    bytes::{be_f64, be_i16, be_u8},
    combinator::slice,
    GenError, GenResult, SerializeFn, WriteContext,
};

use crate::elements::{Lacing, SimpleBlock};

pub trait EbmlDefault<const ID: u32, T> {
    fn default() -> T;
}

/// This trait allows Rust data types to be serialized into
/// a valid representation of certain [EBML Element Types].
///
/// [EBML Element Types]: https://www.rfc-editor.org/rfc/rfc8794.html#name-ebml-element-types
pub trait EbmlSerializable<const ID: u32, T>: Sized {
    /// Serializes the Rust value as zero or more EBML Elements made
    /// up of an Element ID, the Element Data Size and the Element Data.
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W>;

    /// Calculates the exact Element Data Size in bytes/octets.
    fn data_size(&self) -> usize;

    /// Calculates the exact size of the entire EBML Element, including Element ID,
    /// Element Data Size and Element Data in bytes/octets.
    fn size(&self) -> usize {
        let data_sz = self.data_size();
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, T: EbmlDefault<ID, u64>> EbmlSerializable<ID, T> for u64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        if *self == T::default() {
            return Ok(w);
        }

        let sz = <u64 as EbmlSerializable<ID, T>>::data_size(self);
        let w = vid::<ID, W>(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }

    fn data_size(&self) -> usize {
        match self {
            s if *s == T::default() => 0,
            0 => 1, // at least one byte to serialize a zero
            s => size_of::<Self>() - (s.leading_zeros() / 8) as usize,
        }
    }

    fn size(&self) -> usize {
        if *self == T::default() {
            return 0;
        }

        let data_sz = <u64 as EbmlSerializable<ID, T>>::data_size(self);
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, T: EbmlDefault<ID, u32>> EbmlSerializable<ID, T> for u32 {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        if *self == T::default() {
            return Ok(w);
        }

        let sz = <u32 as EbmlSerializable<ID, T>>::data_size(self);
        let w = vid::<ID, W>(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[4 - sz..])(w)
    }

    fn data_size(&self) -> usize {
        match self {
            s if *s == T::default() => 0,
            0 => 1, // at least one byte to serialize a zero
            s => size_of::<Self>() - (s.leading_zeros() / 8) as usize,
        }
    }

    fn size(&self) -> usize {
        if *self == T::default() {
            return 0;
        }

        let data_sz = <u32 as EbmlSerializable<ID, T>>::data_size(self);
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, T: EbmlDefault<ID, i64>> EbmlSerializable<ID, T> for i64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        if *self == T::default() {
            return Ok(w);
        }

        let sz = <i64 as EbmlSerializable<ID, T>>::data_size(self);
        let w = vid::<ID, W>(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }

    fn data_size(&self) -> usize {
        match self {
            i if *i == T::default() => 0,
            i @ 1.. => size_of::<Self>() - (i.leading_zeros() / 8) as usize,
            0 => 1,
            i => size_of::<Self>() - ((i.leading_ones() - 1) / 8) as usize,
        }
    }

    fn size(&self) -> usize {
        if *self == T::default() {
            return 0;
        }

        let data_sz = <i64 as EbmlSerializable<ID, T>>::data_size(self);
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, T: EbmlDefault<ID, f64>> EbmlSerializable<ID, T> for f64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        if *self == T::default() {
            return Ok(w);
        }

        let w = vid::<ID, W>(w)?;
        let w = vint(8)(w)?;
        be_f64(*self)(w)
    }

    fn data_size(&self) -> usize {
        if *self == T::default() {
            0
        } else {
            // TODO: Handle 4-byte floats. Probably needs a newtype
            8
        }
    }

    fn size(&self) -> usize {
        if *self == T::default() {
            return 0;
        }

        let data_sz = <f64 as EbmlSerializable<ID, T>>::data_size(self);
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, T: EbmlDefault<ID, i64>> EbmlSerializable<ID, T> for crate::ebml::Date {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        // The default for Date Elements defaults to Date(0_i64).
        // The default for Integer Elements is also 0, so this is fine.
        <i64 as EbmlSerializable<ID, T>>::serialize(&self.0, w)
    }

    fn data_size(&self) -> usize {
        <i64 as EbmlSerializable<ID, T>>::data_size(&self.0)
    }

    fn size(&self) -> usize {
        if self.0 == T::default() {
            return 0;
        }

        let data_sz = <i64 as EbmlSerializable<ID, T>>::data_size(&self.0);
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, T: EbmlDefault<ID, String>> EbmlSerializable<ID, T> for String {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        if self == &T::default() {
            return Ok(w);
        }

        <&[u8] as EbmlSerializable<ID, _>>::serialize(&self.as_bytes(), w)
    }

    fn data_size(&self) -> usize {
        if self == &T::default() {
            return 0;
        }

        self.len()
    }

    fn size(&self) -> usize {
        if self == &T::default() {
            return 0;
        }

        let data_sz = <String as EbmlSerializable<ID, T>>::data_size(self);
        vid_size::<ID>() as usize + vint_size(data_sz as u64).unwrap() as usize + data_sz
    }
}

impl<const ID: u32, const N: usize> EbmlSerializable<ID, [u8; N]> for [u8; N] {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        <&[u8] as EbmlSerializable<ID, _>>::serialize(&self.as_slice(), w)
    }

    fn data_size(&self) -> usize {
        N
    }
}

impl<const ID: u32, T: EbmlSerializable<ID, T>> EbmlSerializable<ID, Vec<T>> for Vec<T> {
    fn serialize<W: Write>(&self, mut w: WriteContext<W>) -> GenResult<W> {
        for t in self {
            w = <T as EbmlSerializable<ID, T>>::serialize(t, w)?;
        }

        Ok(w)
    }

    fn data_size(&self) -> usize {
        // This is just implemented for completeness' sake.
        self.iter().map(|t| t.data_size()).sum()
    }

    fn size(&self) -> usize {
        if self.is_empty() {
            return 0;
        }

        let id_size = vid_size::<ID>();
        let sizes_and_data = self.iter().map(|t| t.data_size()).fold(0, |acc, sz| {
            acc + sz + vint_size(sz as u64).unwrap() as usize
        });

        self.len() * id_size as usize + sizes_and_data
    }
}

impl<const ID: u32, T: EbmlSerializable<ID, T>> EbmlSerializable<ID, Option<T>> for Option<T> {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        match self {
            Some(t) => <T as EbmlSerializable<ID, T>>::serialize(t, w),
            None => Ok(w),
        }
    }

    fn data_size(&self) -> usize {
        match self {
            Some(t) => t.data_size(),
            None => 0,
        }
    }

    fn size(&self) -> usize {
        match self {
            Some(t) => t.size(),
            None => 0,
        }
    }
}

impl<const ID: u32> EbmlSerializable<ID, Vec<u8>> for Vec<u8> {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        <&[u8] as EbmlSerializable<ID, _>>::serialize(&self.as_slice(), w)
    }

    fn data_size(&self) -> usize {
        self.len()
    }
}

impl<'a, const ID: u32> EbmlSerializable<ID, &'a [u8]> for &'a [u8] {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        let w = vid::<ID, W>(w)?;
        let w = vint(self.len() as u64)(w)?;
        slice(self)(w)
    }

    fn data_size(&self) -> usize {
        self.len()
    }
}

impl<const ID: u32> EbmlSerializable<ID, uuid::Uuid> for uuid::Uuid {
    fn serialize<W: Write>(&self, w: WriteContext<W>) -> GenResult<W> {
        <[u8; 16] as EbmlSerializable<ID, _>>::serialize(self.as_bytes(), w)
    }

    fn data_size(&self) -> usize {
        16
    }
}

pub fn vint_size(val: u64) -> Result<u8, GenError> {
    match val {
        // (1 << 56) - 1 is the maximum value that can fit into an
        // 8-byte VINT. Anything larger is currently not supported.
        v if v >= (1 << 56) => Err(GenError::NotYetImplemented),
        0 => Ok(1),
        v => Ok(1 + v.ilog2() as u8 / 7),
    }
}

pub fn vint<W: Write>(val: u64) -> impl SerializeFn<W> {
    move |output| {
        let size = vint_size(val)?;
        let shortened = &mut val.to_be_bytes()[8 - size as usize..];

        // set VINT_MARKER
        shortened[0] |= 1 << (8 - size);

        // disable incorrect clippy warning
        #[allow(clippy::let_and_return)]
        let r = slice(shortened)(output);
        r
    }
}

pub fn vid<const ID: u32, W: Write>(w: WriteContext<W>) -> GenResult<W> {
    assert_ne!(ID, 0);
    let skip = 4 - vid_size::<ID>() as usize;

    slice(&ID.to_be_bytes()[skip..])(w)
}

pub const fn vid_size<const ID: u32>() -> u8 {
    4 - (ID.leading_zeros() / 8) as u8
}

pub fn segment_element<W: Write>(w: WriteContext<W>) -> GenResult<W> {
    let w = vid::<0x18538067, W>(w)?;
    be_u8(0xFF)(w)
}

pub(crate) fn simple_block_header<W: Write>(s: &SimpleBlock) -> impl SerializeFn<W> + '_ {
    move |w| {
        let w = vint(s.track_number)(w)?;
        let w = be_i16(s.timestamp)(w)?;

        let mut flags = 0;

        if s.keyframe {
            flags |= 0b0000_0001;
        }

        if s.invisible {
            flags |= 0b0001_0000;
        }

        flags |= match s.lacing {
            Lacing::None => 0,
            Lacing::Xiph => 0b0010_0000,
            Lacing::FixedSize => 0b0100_0000,
            Lacing::EBML => 0b0110_0000,
        };

        if s.discardable {
            flags |= 0b1000_0000;
        }

        be_u8(flags)(w)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vint_size_correct() {
        let tests: [(u64, Option<u8>); 7] = [
            (0, Some(1)),
            (100, Some(1)),
            ((1 << 7) - 1, Some(1)),
            (1 << 7, Some(2)),
            ((1 << 56) - 1, Some(8)),
            (1 << 56, None),
            (u64::MAX, None),
        ];

        for (val, expected_size) in tests {
            let actual_size = vint_size(val).ok();
            assert_eq!(
                actual_size, expected_size,
                "VINT size of value {val} is {actual_size:?}, but should be {expected_size:?}."
            );
        }
    }

    #[test]
    fn vint_correct() {
        #[rustfmt::skip]
        let tests: [(u64, Option<[u8; 8]>); 8] = [
            (0, Some([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (1, Some([0x81, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (127, Some([0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (128, Some([0x40, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (1 << 35, Some([0x04, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            ((1 << 56) - 1, Some([0x01, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
            (1 << 56, None),
            (u64::MAX, None),
        ];

        for (val, expected_output) in tests {
            let mut buf = [0u8; 8];
            let w = buf.as_mut_slice().into();

            // If vint() fails, expected_output == None
            assert_eq!(vint(val)(w).is_ok(), expected_output.is_some());

            if let Some(expected_buf) = expected_output {
                assert_eq!(buf, expected_buf);
            }
        }
    }

    impl EbmlDefault<0xD8, u64> for u64 {
        fn default() -> u64 {
            0
        }
    }

    impl EbmlDefault<0xABCD_EFFE, u64> for u64 {
        fn default() -> u64 {
            42
        }
    }

    #[test]
    fn uint_correct() {
        // To explain the expected outputs, let's use the third test as an example:
        // (val: 0xFF, expected_len: 3, id: 0xD8) should result in: [0xD8, 0x81, 0xFF, 0x00, /* more zeros */],
        //
        // The first byte 0x84 is the Element ID literally.
        // The second byte 0x81 is a 1-byte long VINT representing the Element Size:
        // 0b1000_0001 => The most significant ("first") bit is the VINT_MARKER, which
        // shows us that the VINT is one octet/byte long. All bits after that are inter-
        // preted as the uint value. So in this case that would be 0b000_0001, so just 1.
        //
        // This shows us that we need to "take" one more octet/byte to get the value of this
        // uint Element. So we just take the next byte (0xFF) and interpret that literally.
        //
        // If we need to "take" more than one octet/byte, we do it in Big-Endian order.
        // For us, this just means "read/write the arrays/slices in order", so index ascending.

        // max length = 4 (ID) + 1 (Size) + 8 (Data) = 13
        #[rustfmt::skip]
        let tests = [
            (0, 0, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (1, 3, [0xD8, 0x81, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (0xFF, 3, [0xD8, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
        ];

        for (val, expected_len, expected) in tests {
            check_uint::<0xD8, u64>(val, expected_len, expected);
        }

        #[rustfmt::skip]
        let tests = [
            ((4321 << 42) + 8765, 12, [0xAB, 0xCD, 0xEF, 0xFE, 0x87, 0x43, 0x84, 0x00, 0x00, 0x00, 0x22, 0x3D, 0x00]),
            (42, 0, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (u64::MAX, 13, [0xAB, 0xCD, 0xEF, 0xFE, 0x88, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
        ];

        for (val, expected_len, expected) in tests {
            check_uint::<0xABCD_EFFE, u64>(val, expected_len, expected);
        }
    }

    fn check_uint<const ID: u32, T: EbmlDefault<ID, u64>>(
        val: u64,
        expected_len: u64,
        expected: [u8; 13],
    ) {
        let mut buf = [0u8; 13];
        let w = buf.as_mut_slice().into();

        let w = <u64 as EbmlSerializable<ID, T>>::serialize(&val, w)
            .unwrap_or_else(|_| panic!("serialization failed for id: {ID:#0X}"));

        let written = w.position;
        assert_eq!(buf, expected, "id: {ID:#0X}");
        assert_eq!(written, expected_len, "id: {ID:#0X}");
    }

    impl EbmlDefault<0xE9, i64> for i64 {
        fn default() -> i64 {
            0
        }
    }

    impl EbmlDefault<0x12AB_34CD, i64> for i64 {
        fn default() -> i64 {
            -17
        }
    }

    #[test]
    fn int_correct() {
        // see uint_correct for an explanation of what's happening here
        #[rustfmt::skip]
        let tests = [
            // x >= 0 -> equal to uint:
            (0, 0, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (1, 3, [0xE9, 0x81, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (0xFF, 3, [0xE9, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),

            // x < 0 -> more tests
            (i64::MIN, 10, [0xE9, 0x88, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-1, 3, [0xE9, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-128, 3, [0xE9, 0x81, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-129, 4, [0xE9, 0x82, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-87654321, 6, [0xE9, 0x84, 0xFA, 0xC6, 0x80, 0x4F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((-4321 << 42) - 8765, 9, [0xE9, 0x87, 0xBC, 0x7B, 0xFF, 0xFF, 0xFF, 0xDD, 0xC3, 0x00, 0x00, 0x00, 0x00]),
        ];

        for (val, expected_len, expected) in tests {
            check_int::<0xE9, i64>(val, expected_len, expected);
        }

        // long IDs
        #[rustfmt::skip]
        let tests = [
            (-17, 0, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((4321 << 42) + 8765, 12, [0x12, 0xAB, 0x34, 0xCD, 0x87, 0x43, 0x84, 0x00, 0x00, 0x00, 0x22, 0x3D, 0x00]),
            (i64::MAX, 13, [0x12, 0xAB, 0x34, 0xCD, 0x88, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
        ];

        for (val, expected_len, expected) in tests {
            check_int::<0x12AB_34CD, i64>(val, expected_len, expected)
        }
    }

    fn check_int<const ID: u32, T: EbmlDefault<ID, i64>>(
        val: i64,
        expected_len: u64,
        expected: [u8; 13],
    ) {
        let mut buf = [0u8; 13];
        let w = buf.as_mut_slice().into();
        let w = <i64 as EbmlSerializable<ID, T>>::serialize(&val, w)
            .unwrap_or_else(|_| panic!("serialization failed for id: {ID:#0X}"));

        let written = w.position;
        assert_eq!(buf, expected, "id: {ID:#0X}");
        assert_eq!(written, expected_len, "id: {ID:#0X}");
    }
}
