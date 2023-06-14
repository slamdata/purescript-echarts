export function unsafeSetField(obj) {
    return function(key) {
        return function(val) {
            obj[key] = val;
            return obj;
        };
    };
};

export function emptyObject() {
    return {};
};

export const undefinedValue = undefined;
