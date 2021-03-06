function initMap() {
    var app = Elm.Main.embed(document.getElementById("elm-area"));
    var map = document.getElementById("map-area");
    var lat = 43.03;
    var lng = -87.94;
    var mapOptions = {
        zoom: 13,
        center: new google.maps.LatLng(lat,lng),
        disableDoubleClickZoom: true,
        draggable: false
        // mapTypeId: google.maps.MapTypeId.HYBRID
    };
    var scale = 0.03;
    var resolution = 1000;
    var timesCalled = 0;

    var currentWaypoints = null;
    var pastWaypoints = null;
    var markers = null;
    var startMarker = null;
    var gmap = null;
    var startPos = null;
    var directionsService = null;
    var directionsDisplay = null;

    function reinit() {
        currentWaypoints = {};
        pastWaypoints = {};
        markers = {};
        startMarker = null;
        gmap = new google.maps.Map(map, mapOptions);
        startPos = null;
        directionsService = new google.maps.DirectionsService;
        directionsDisplay = new google.maps.DirectionsRenderer({preserveViewport: true,
                                                                suppressMarkers: true});
        directionsDisplay.setMap(gmap);

   }
    reinit();

    var redoDirections = function() {
        if(startPos !== null && areSymDiff(pastWaypoints, currentWaypoints)) {
            var directionsReq = {
                origin: startPos,
                destination: startPos,
                travelMode: 'DRIVING',
                waypoints: Object.keys(currentWaypoints).map(function(key){return currentWaypoints[key];}),
                optimizeWaypoints: true
            };
            timesCalled++;
            if(timesCalled % 10 == 0) {
                console.log("Called " + timesCalled + "times.");
            }
            if(timesCalled > 2500) {
                console.log("Over query limit!");
            }
            directionsService.route(directionsReq, directionCallback(directionsDisplay));

            pastWaypoints = {};
            for(var key in currentWaypoints) {
                pastWaypoints[key] = currentWaypoints[key];
            }
        }
    };

    app.ports.addHub.subscribe(function(pos) {
        startPos = pos;
        startMarker = new google.maps.Marker({
            position: pos,
            map: gmap,
            icon: "Google Maps Markers/brown_MarkerP.png"
        });
    });

    app.ports.clearTanks.subscribe(function(x) {
        reinit();
    });

    app.ports.addTank.subscribe(function(posyr) {
        // console.log(posyr);
        var id = posyr[0];
        var pos = posyr[1];
        var yellow = posyr[2];
        var red = posyr[3];
        var marker=new google.maps.Marker({
            position:pos,
            map: gmap
        });
        marker.addListener('click', markerCallback(app, id));
        marker.addListener('dblclick', markerDblCallback(app, id));
        // setInterval(sendDataCallback(app, id, redoDirections), resolution);
        markers[id] = marker;
        app.ports.addMarker.send(posyr);
    });

    app.ports.setColor.subscribe(function(val) {
        var chart = val[0];
        var color = val[1];
        var manual = val[2];
        if(color == "empty") {
            color = "pink";
        } else if(color == "noreadings") {
            color = "blue";
        }
        var letter = manual ? "M" : "T";
        markers[chart].set("icon","Google Maps Markers/"+color+"_Marker"+letter+".png");
    });

    app.ports.sendRoutes.subscribe(function(val) {
        currentWaypoints = {};
        for(var r in val.manual) {
            var manual = val.manual[r];
            var marker = markers[manual];
            currentWaypoints[manual] = {'location': marker.position, stopover: true};
        }
        for(var r in val.noreadings) {
            var none = val.noreadings[r];
            var marker = markers[none];
            currentWaypoints[none] = {'location': marker.position, stopover: true};
        };
        for(var r in val.low) {
            var low = val.low[r];
            var marker = markers[low];
            currentWaypoints[low] = {'location': marker.position, stopover: true};
        };
        redoDirections();
    });

    // app.ports.sendChartVal.subscribe(function(vals) {
    //     var lowMarkers = {};
    //     currentWaypoints = {};
    //     for(var i = 0; i < vals.length; i++) {
    //         var k = vals[i].id;
    //         currentWaypoints[k] = {location: markers[k].position, stopover: true};
    //         if(markers[k].getAnimation() == null) {
    //             markers[k].setAnimation(google.maps.Animation.BOUNCE);
    //         }
    //         lowMarkers[k] = true;
    //     }
    //     for(var m in markers) {
    //         if(!lowMarkers.hasOwnProperty(m)) {
    //             markers[m].setAnimation(null);
    //         }
    //     }
    // });

    // setTimeout(function() { setInterval(redoDirections, resolution); }, 400);
}


function directionCallback(display) {
    return function(response, status) {
        if(status == "OK") {
            display.setDirections(response);
        } else {
            console.log(status, response);
        }
    };
}

function markerCallback(app, id) {
    return function() {
        app.ports.markerClicked.send(id);
    };
}

function markerDblCallback(app, id) {
    return function() {
        app.ports.markerDblClicked.send(id);
        // if(manualClick.hasOwnProperty(id)) {
        //     delete manualClick[id];
        // } else {
        //     manualClick[id] = true;
        // }
    };
}


function sendDataCallback(app, id, displaycb) {
    return function() {
        var value = Math.random()*3 - 1.6;
        app.ports.updateMarker.send({id: id, value: value});
    };
}

function areSymDiff(map1, map2) {
    for(var key1 in map1) {
        if(!(key1 in map2)) {
            return true;
        }
    }
    for(var key2 in map2) {
        if(!(key2 in map1)) {
            return true;
        }
    }
    return false;
}
