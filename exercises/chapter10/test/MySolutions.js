"use strict";

export function volumeFn(x, y, z) {
  return x * y * z
}

export const volumeArrow = x => y => z => volumeFn(x, y, z)

function complex(real, imag) {
  return { real, imag }
}

function sumComplex(c1, c2) {
  return complex(
    c1.real + c2.real,
    c1.imag + c2.imag,
  )
}

export function cumulativeSumsComplex(arr) {
  return arr.reduce(
    (acc, cur) => {
      const sum = sumComplex(acc.sum, cur)
      const sums = [...acc.sums, sum]
      return { sum, sums }
    },
    { sum: complex(0, 0), sums: [] }
  ).sums;
}

function rootsOfQuadEq(discriminant, a, b) {
  return [
    (-b + Math.sqrt(discriminant)) / (2 * a),
    (-b - Math.sqrt(discriminant)) / (2 * a),
  ];
}

export const quadraticRootsImpl = pair => quad => {
  const { a, b, c } = quad;
  const discriminant = b * b - 4 * a * c
  if (discriminant > 0) {
    const [r1, r2] = rootsOfQuadEq(discriminant, a, b)
    return pair(complex(r1, 0))(complex(r2, 0))
  } else if (discriminant === 0) {
    const r = -b / (2 * a)
    const c = complex(r, 0)
    return pair(c)(c)
  } else {
    const real = -b / (2 * a)
    const imag = Math.sqrt(-discriminant) / (2 * a)
    return pair(complex(real, imag))(complex(real, -imag))
  }
}

export const toMaybeImpl = just => noth => isUndef => val => isUndef(val)?noth:just(val)

export function valuesOfMapJson(json) {
  const map = new Map(json);
  // jsonDecode knows that uni-dimensional arrays
  // should be decoded as a Set by the function type
  return Array.from(map.values());
}
