# HZip

The goal of this project is to implement a lossless data compression algorithm in Haskell.

## LZ77

The standard LZ77 implementation uses two bytes for each pair. The first byte is either a special flag which indicates the following byte is a literal or a <distance, length> pair which tells us how to decode the string.

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

The LZSS algorithm is very similar to the LZ77 implementation but further reduces file size at the cost of compression times. Rather than using a reserved byte to indicate a literal, a single bit is used where 1 is a literal and 0 is a <distance, length> pair.

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

## Dependencies

[Provides several functions to quickly search for substrings in strict or lazy ByteStrings.](https://hackage.haskell.org/package/stringsearch)

```bash
cabal install stringsearch
```
