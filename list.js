// @ts-check

/**
 * @template T
 * @param {T[]} list
 * @returns {T[]}
 */
export function shuffle(list) {
    for (let i = list.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1));
        [list[i], list[j]] = [list[j], list[i]];
    }
    return list
}

/**
 * @template T
 * @param {number} n
 * @param {(i: number) => T} next
 * @returns {T[]}
 */
export function generate(n, next) {
    /** @type {T[]} */
    let res = [];
    for (let i = 0; i < n; i++) {
      res.push(next(i));
    }
    return res;
}

/**
 * @template T
 * @template R
 * @param {T[]} collection 
 * @param {(x: T, i: number) => [R, boolean]} f 
 * @returns {R[]}
 */
export function filterMap( collection, f) {
    /** @type {R[]} */
    let res = [];
    for (let i = 0; i < collection.length; i++) {
        const [y, ok] = f(collection[i], i);
        if (!ok) {
            continue;
        }
        res.push(y);
    }
    return res;
}
