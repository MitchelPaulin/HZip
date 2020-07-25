# HZip

The goal of this project is to implement the DEFLATE algorithm is Haskell. The DEFLATE algorithm is a popular lossless compression algorithm used by other compression tools such as gzip. The DEFLATE algorithm is divided into two parts, first an LZ77 encoding is performed, then the encoded values are encoded again via a Huffman encoding. This means as a consequence you get a working implementation of an LZ77 encoder.

## LZ77

You can use the standard LZ77 implementation which uses two bytes for each pair. The first byte is either a special flag which indicates the following byte is a literal or a <distance, length> pair which tells us how to decode the string.

To compile

```bash
./build.sh
```

To encode

```bash
./lz77.hs -e <filepath>
```

This will produce a **.hzip** file with the same name as the original file.

To decode

```bash
./lz77.hs -d <filepath>
```

## LZSS

The LZSS algorithm is very similar to the LZ77 implementation but further reduces file size at the cost of compression times. Rather than using a reserved byte to indicate a literal, a single bit is used.

To compile

```bash
./build.sh
```

To encode

```bash
./lzss.hs -e <filepath>
```

This will produce a **.hzip** file with the same name as the original file.

To decode

```bash
./lzss.hs -d <filepath>
```

## Deflate

WIP

## Dependencies

[Provides several functions to quickly search for substrings in strict or lazy ByteStrings.](https://hackage.haskell.org/package/stringsearch)

```bash
cabal install stringsearch
```
