# HideAndScript

HideAndScript is a Haskell program that can hide encrypted Python programs inside a PNG image.

It accomplishes this by storing data inside images using LSB steganography. To keep things short, the least significant bit (LSB) of all pixels is concatenated together, revealing a secret message.

In HideAndScript, this secret message is an encrypted structure, employing Curve25519 ECC keys, the XSalsa20 stream cipher and an HMAC signature. Only images created using the same `curve25519.secret` secret key can be decrypted.

HideAndScript can also detect images that do not contain any secret data written using the same secret key (error handling).

## Installation

First, decide whether you wish to download the latest nightly release from [GitHub Actions](https://nightly.link/darktohka/HideAndScript/workflows/autobuild-workflow.yaml/master) or build the executable yourself.

## Building

To build the executable, first clone the repository:

```
git clone https://github.com/darktohka/HideAndScript
cd HideAndScript
```

Then, build and run the executable:

```
stack run
```
