export function shuffle<T>(list: T[]): T[] {
    for (let i = list.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1));
        [list[i], list[j]] = [list[j], list[i]];
    }
    return list
}

export function generate<T>(n: number, next: () => T): T[] {
    let res: T[] = [];
    for (let i = 0; i < n; i++) {
      res.push(next());
    }
    return res;
}

export function filterMap<T, R>(
    collection: T[],
    f: (x: T, i: number) => [R, boolean]
): R[] {
    let res: R[] = [];
    for (let i = 0; i < collection.length; i++) {
        const [y, ok] = f(collection[i], i);
        if (!ok) {
            continue;
        }
        res.push(y);
    }
    return res;
}
