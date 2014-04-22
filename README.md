# Tropical

Tropical numbers are the same as real numbers, except the operations are
different. You can compound these operations, and get tropical vectors,
tropical matrices, etc. This package is a Haskell library dealing with
tropical things.

## Formal definition of tropicality

The tropical semiring is $\{\mathbb{R},\, \infty,\, \oplus,\, \odot\}$.
The scalar operations are defined as such:

$$a \oplus b = min\;\{a,\, b\}$$
$$a \odot b = a + b$$

Note the difference between $+$ and $\oplus$. Also note, since there is
no inverse classical minimum, (classical maximum doesn’t count), you
cannot tropical subtract. There is, however, inverse classical addition,
so you can tropical divide (classical subtract).

When coding, typing $\oplus$ and $\odot$ would require some fancy tricks with
Unicode. Because of this, we denote them `<+>` and `<*>`, respectively.

## Contributing

Anyone who wants to contribute is more than welcome. This library is
BSD-licensed, so you can pretty much do what you want with it.
