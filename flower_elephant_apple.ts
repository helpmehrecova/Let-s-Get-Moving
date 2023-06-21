let startMoving: boolean = false;

let speed: number = 0;

function increaseSpeed(): void {
    if (startMoving) {
        speed += 10;
        if (speed > 60) {
            speed = 60;
        }
    }
}

function decreaseSpeed(): void {
    if (startMoving) {
        speed -= 10;
        if (speed < 0) {
            speed = 0;
        }
    }
}

function turnOn(): void {
    startMoving = true;
}

function turnOff(): void {
    startMoving = false;
    speed = 0;
}

function startCar(): void {
    if (!startMoving) {
        increaseSpeed();
    }
}

function stopCar(): void {
    if (startMoving) {
        decreaseSpeed();
    }
}

function getSpeed(): number {
    return speed;
}

function reportSpeed(): void {
    console.log(`The current speed is ${getSpeed()}.`);
}

function handleGas(accelerate: boolean): void {
    if (accelerate) {
        increaseSpeed();
    } else {
        decreaseSpeed();
    }
}

function shiftGear(gear: string): void {
    console.log(`Shifting gear to ${gear}`);
}

function checkFuelLevel(): boolean {
    const hasFuel: boolean = Math.random() >= 0.5;
    if (hasFuel) {
        console.log("You have enough fuel.");
    } else {
        console.log("You don't have enough fuel.");
    }
    return hasFuel;
}

function serviceWarning(): void {
    const needsService: boolean = Math.random() >= 0.5;
    if (needsService && checkFuelLevel()) {
        console.log("The car needs servicing.");
    } else {
        console.log("The car is in perfect condition.");
    }
}

function checkEngine(): void {
    console.log("Checking engine...");
    if (startMoving && getSpeed() > 0) {
        console.log("Engine is running.");
    } else {
        console.log("Engine is not running.");
    }
}

function adjustMirrors(): void {
    console.log("Adjusting mirrors...");
}

function adjustSeats(): void {
    console.log("Adjusting seats...");
}

function checkBrakes(): void {
    console.log("Checking brakes...");
}

function checkLights(): void {
    console.log("Checking lights...");
}

function checkTires(): void {
    console.log("Checking tires...");
}

function checkFluids(): void {
    console.log("Checking fluids...");
}

function checkBattery(): void {
    console.log("Checking battery...");
}

function checkOil(): void {
    console.log("Checking oil...");
}

function checkExhaust(): void {
    console.log("Checking exhaust...");
}

function checkAirFilter(): void {
    console.log("Checking air filter...");
}

function checkWindshieldWipers(): void {
    console.log("Checking windshield wipers...");
}

function inspectCar(): void {
    adjustMirrors();
    adjustSeats();
    checkBrakes();
    checkLights();
    checkTires();
    checkFluids();
    checkBattery();
    checkOil();
    checkExhaust();
    checkAirFilter();
    checkWindshieldWipers();
    serviceWarning();
    checkEngine();
}

function getReadyToGo(): void {
    turnOn();
    inspectCar();
    startCar();
    reportSpeed();
}

function acknowledgeSpeed(): void {
    if (getSpeed() > 0) {
        console.log("We're moving.");
    } else {
        console.log("We're not moving yet.");
    }
}

function letsGetMoving(): void {
    getReadyToGo();
    acknowledgeSpeed();
}

letsGetMoving();