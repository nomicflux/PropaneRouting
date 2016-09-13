function initMap() {
    var currentWaypoints = {};
    var pastWaypoints = {};
    var markers = {};

    var app = Elm.Propane.embed(document.getElementById("elm-area"));

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
    var gmap = new google.maps.Map(map, mapOptions);
    var scale = 0.03;

    var startPos = new google.maps.LatLng({lat: lat + Math.random()*2*scale - scale, lng: lng + Math.random()*2*scale - scale});
    var startMarker = new google.maps.Marker({
        position: startPos,
        map: gmap,
        icon: "Google Maps Markers/blue_MarkerP.png"
    });

    var directionsDisplay = new google.maps.DirectionsRenderer({preserveViewport: true});
    directionsDisplay.setMap(gmap);
    var directionsService = new google.maps.DirectionsService;

    var timesCalled = 0;
    var redoDirections = function() {
        if(areSymDiff(pastWaypoints, currentWaypoints)) {
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

    app.ports.sendChartVal.subscribe(function(vals) {
        var lowMarkers = {};
        currentWaypoints = {};
        for(var i = 0; i < vals.length; i++) {
            var k = vals[i].pos.lat.toString() + "," + vals[i].pos.lng.toString();
            currentWaypoints[k] = {location: vals[i].pos, stopover: true};
            if(markers[k].getAnimation() == null) {
                markers[k].setAnimation(google.maps.Animation.BOUNCE);
            }
            lowMarkers[k] = true;
        }
        for(var m in markers) {
            if(!lowMarkers.hasOwnProperty(m)) {
                markers[m].setAnimation(null);
            }
        }
    });

    var resolution = 1000;
    for(var i = 0; i < 5; i++) {
        var newLat = lat + Math.random()*2*scale - scale;
        var newLng = lng + Math.random()*2*scale - scale;
        var newPos = {lat: newLat, lng: newLng};
        var marker=new google.maps.Marker({
            position:newPos,
            map: gmap
        });
        app.ports.addMarker.send(newPos);
        marker.addListener('click', markerCallback(app, newPos));
        setInterval(sendDataCallback(app, newPos, i, newPos, redoDirections), resolution);
        var k = newLat.toString() + "," + newLng.toString();
        markers[k] = marker;
    }
    setTimeout(function() { setInterval(redoDirections, resolution); }, 400);
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

function markerCallback(app, pos) {
    return function() {
        app.ports.markerClicked.send(pos);
    };
}

function sendDataCallback(app, pos, id, latlng, displaycb) {
    return function() {
        var value = Math.random()*3 - 1.6;
        app.ports.updateMarker.send({pos: pos, value: value});
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
