// define variable to contain R binding methods
const methods = {};

HTMLWidgets.widget({
  name: 'cesium',
  type: 'output',

  factory: function(el, width, height) {

    let globe = null;

    return {
      renderValue: function(data) {
        const id = el.id
        const options = data.options
        const globe = new Cesium.Viewer(id, options);

        for (var i = 0; data.calls && i < data.calls.length; i++) {
          let call = data.calls[i];
          if (methods[call.method]) methods[call.method].apply(globe, call.args);else (0, _util.log)("Unknown method " + call.method);
        }
      },
    };
  }
});

methods.addCZML = function(path, layerid) {
  const globe = this;
  const promise = Cesium.CzmlDataSource.load(path);
  const dataSource = globe.dataSources.add(promise);
  globe.zoomTo(dataSource);
  console.log(dataSource);
}

methods.flyTo = function(destination, duration) {
  var globe = this;
  var camera = globe.camera;
  camera.flyTo({
    destination: Cesium.Cartesian3.fromDegreesArrayHeights(destination)[0],
    duration: duration
  });
}

methods.setView = function(destination) {
  var globe = this;
  var camera = globe.camera;
  camera.setView({destination: Cesium.Cartesian3.fromDegreesArrayHeights(destination)[0]});
}

methods.setClock = function(options) {
  const globe = this;
  const clock = globe.clock;
  const start =  Cesium.JulianDate.fromIso8601(options.startTime);
  const current = Cesium.JulianDate.fromIso8601(options.currentTime);
  const stop =  Cesium.JulianDate.fromIso8601(options.stopTime);
  clock.startTime = start;
  clock.currentTime = current;
  clock.stopTime = stop;
  clock.clockRange = options.clockRange;
  clock.clockStep = options.clockStep;
  clock.multiplier = 10519200;
  console.log(clock);
  globe.timeline.zoomTo(current, start);
}


