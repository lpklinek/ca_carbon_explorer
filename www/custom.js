// Clear all Leaflet Draw items (_drawnItems)
Shiny.addCustomMessageHandler("clearDrawnItems", function(message) {
  if(window.map && window.map._drawnItems){
    window.map._drawnItems.clearLayers();
  }
});