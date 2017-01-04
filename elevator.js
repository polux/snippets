{
    init: function(elevators, floors) {

        var lessBusy = function() {
            var result = elevators[0];
            elevators.slice(1).forEach(function (elevator) {
                if (elevator.queue.length < result.queue.length) {
                    result = elevator;
                }
            });
            return result;
        };
        
        var enqueue = function(elevator, floor) {
          if (!elevator.queue.includes(floor)) {
              elevator.queue.push(floor);
          }
        }
                
        elevators.forEach(function (elevator) {
          elevator.queue = [];
          elevator.on("floor_button_pressed", function (floor) {
            enqueue(elevator, floor);
          });
          elevator.on("idle", function () {
              console.log("here: " + elevator.queue);
              if (elevator.queue.length > 0) {
                elevator.goToFloor(elevator.queue.shift());
              }
          });
        });
        
        floors.forEach(function (floor) {
          floor.on("up_button_pressed", function () {
            var elevator = lessBusy(elevators);
            enqueue(elevator, floor.floorNum());
          });
          floor.on("down_button_pressed", function () {
            var elevator = lessBusy(elevators);
            enqueue(elevator, floor.floorNum());
          });
        });
        
    },
    update: function(dt, elevators, floors) {
        // We normally don't need to do anything here
    }
}
