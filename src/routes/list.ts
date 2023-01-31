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
    f: (x: T) => [R, boolean]
): R[] {
    let res: R[] = [];
    for (let x of collection) {
        const [y, ok] = f(x);
        if (!ok) {
            continue;
        }
        res.push(y);
    }
    return res;
}
