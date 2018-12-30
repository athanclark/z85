# z85

[z85](https://rfc.zeromq.org/spec:32/Z85/) is a binary string codec, like hexadecimal or base64, but has a higher density
of compression than the former, due to its use of a higher base value of 85 than base 64. ByteStrings just need to be
a length of a multiple of 4 (a Word32String might be a better name).

There are multiple layers of exposed implementation in this package

- `Word32 <-> Vector 4 Z85Char` for low level work
- Attoparsec `Parser ByteString <-> Parser Text` for slightly higher level parsing of strict data
- Pipes `Pipe ByteString Text <-> Pipe Text ByteString` for encoding / decoding streams of strict data
- Casual `Lazy.ByteString ~ Lazy.Text` functions for encoding / decoding lazy data.
