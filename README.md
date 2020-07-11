# HZip

The goal of this project is to implement the DEFLATE algorithm is Haskell. The DEFLATE algorithm is a popular lossless compression algorithm used by other compression tools such as gzip. 

## LZ77

The DEFLATE algorithm is divided into two parts, first an LZ77 encoding is performed, then the encoded values are encoded again via a Huffman encoding. This means as a consequence you get a working implementation of an LZ77 encoder. 

To compile 
```
ghci lz77.hs
```
To encode 
```
./lz77.hs -e <filepath>
```
To decode 
```
./lz77.hs -d <filepath>
```

## Deflate

WIP