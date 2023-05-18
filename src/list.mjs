// @ts-check

/**
 * Shuffle list in-place.
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
 * Generate list using given generator-by-index function.
 * @template T
 * @param {number} n - length of resulting list
 * @param {(i: number) => T} next - generator-by-index funtion
 * @returns {T[]}
 */
export function generate(n, next) {
    // @type {T[]}
    let res = [];
    for (let i = 0; i < n; i++) {
      res.push(next(i));
    }
    return res;
}

/**
 * Map and filter list using given function.
 * @template T, R
 * @param {T[]} collection - source list
 * @param {(elem: T, i: number) => [R, boolean]} f - mapper, returning result and bool, whether to leave result
 * @returns {R[]}
 */
export function filterMap(collection, f) {
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
