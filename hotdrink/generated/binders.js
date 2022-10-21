
    function binder(element, value, type) {
        value.value.subscribe({
            next: val => {
                if (val.hasOwnProperty('value')) {
                    element[type] = val.value;
                }
            }
        });
        element.addEventListener('input', () => {
            value.value.set(element[type]);
        });
    }

    export function stringBinder(element, value) {
        binder(element, value, "value");
    }

    export function numberBinder(element, value) {
        binder(element, value, "valueAsNumber");
    }

    export function checkedBinder(element, value) {
        binder(element, value, "checked");
    }
    