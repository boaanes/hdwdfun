
        import { stringBinder, numberBinder, checkedBinder } from "./binders.js";
        import { whap } from "./whapDemo.js";

        const system = new hd.ConstraintSystem();
        window.constraintSystem = system;

        window.onload = () => {
            system.addComponent(whap);
                        numberBinder(document.getElementById('w'), whap.vs.w);
            numberBinder(document.getElementById('h'), whap.vs.h);
            numberBinder(document.getElementById('a'), whap.vs.a);
            numberBinder(document.getElementById('p'), whap.vs.p);

            system.update();
    }
    