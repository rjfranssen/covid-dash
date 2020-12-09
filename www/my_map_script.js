/****************************************************************************
Leaflet js map
https://leafletjs.com/examples/choropleth/
****************************************************************************/
var map_c = L.map('map_div_c').setView([37.1, -95.7], 3);

L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
    maxZoom: 18,
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, ' +
        '<a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
        'Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    id: 'mapbox/light-v9',
    tileSize: 512,
    zoomOffset: -1
}).addTo(map_c);


// control that shows state info on hover
var info = L.control();

info.onAdd = function(map) {
    this._div = L.DomUtil.create('div', 'info');
    this.update();
    return this._div;
};

info.update = function(props) {
    this._div.innerHTML = '<h4>Positive Cases</h4>' + (props ?
        '<b>' + props.NAME + '</b><br />' + props.positive + ' people infected' :
        'Hover over a state');
};

info.addTo(map_c);


// get color depending on property value
function getColor(d) {
    return d > 1000 ? '#800026' :
        d > 500 ? '#BD0026' :
        d > 200 ? '#E31A1C' :
        d > 100 ? '#FC4E2A' :
        d > 50 ? '#FD8D3C' :
        d > 20 ? '#FEB24C' :
        d > 10 ? '#FED976' :
        '#FFEDA0';
}

function style(feature) {
    return {
        weight: 2,
        opacity: 1,
        color: 'white',
        dashArray: '3',
        fillOpacity: 0.7,
        fillColor: getColor(feature.properties.hospitalizedCurrently)
    };
}

function highlightFeature(e) {
    var layer = e.target;

    layer.setStyle({
        weight: 5,
        color: '#667',
        dashArray: '',
        fillOpacity: 0.7
    });

    if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
        layer.bringToFront();
    }

    info.update(layer.feature.properties);
}

var geojson;

function resetHighlight(e) {
    geojson.resetStyle(e.target);
    info.update();
}


function zoomToFeature(e) {
    map.fitBounds(e.target.getBounds());
}

// Add Mouse CLick event
function mouse_click(e) {
    //var ID = this.options.myID;
    //var clickInfo = e.sourceTarget.feature; // works
    var clickInfo = e.sourceTarget.feature.properties.NAME; // works
    //alert("You clicked the marker at " + e.latlng + " ID: " + ID);
    console.log("Selected: ", clickInfo);
    //console.log("mouse_click ID: ", ID);

    // send this ID to the shiny server code through messaging 
    Shiny.onInputChange("clicked", [clickInfo, Math.random()]);
}


function onEachFeature(feature, layer) {
    layer.on({
        mouseover: highlightFeature,
        mouseout: resetHighlight,
        //click: zoomToFeature //going to replace 'zoom to' with my click event
        click: mouse_click
    });
}

geojson = L.geoJson(statesCovidData, {
    style: style,
    onEachFeature: onEachFeature
}).addTo(map_c);

map_c.attributionControl.addAttribution('COVID Tracking data &copy; <a href="https://covidtracking.com/data">COVID Tracking Project</a>');

var legend = L.control({
    position: 'bottomright'
});

legend.onAdd = function(map) {

    var div = L.DomUtil.create('div', 'info legend'),
        grades = [0, 10, 20, 50, 100, 200, 500, 1000],
        labels = [],
        from, to;

    for (var i = 0; i < grades.length; i++) {
        from = grades[i];
        to = grades[i + 1];

        labels.push(
            '<i style="background:' + getColor(from + 1) + '"></i> ' +
            from + (to ? '&ndash;' + to : '+'));
    }

    div.innerHTML = labels.join('<br>');
    return div;
};

legend.addTo(map_c);

// add it to the map
//L.geoJson(statesData, {style: style}).addTo(map_c);


// Issue with tiles not loading completely 
// https://github.com/Leaflet/Leaflet/issues/694
// After 5 seconds, check if the map container size changed and updates the map if so
setTimeout(function() {
    map_c.invalidateSize();
    map_c.setView([37.1, -95.7], 3);
}, 5000);