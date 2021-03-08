$("#reset_net").click(function() {
  $(".display.forceNetwork.no-footer").ForceNetwork().destroy();
  $(".display.forceNetwork.no-footer").ForceNetwork().clear().draw();    
  $(".display.no-footer").ForceNetwork().destroy();
  $(".display.no-footer").ForceNetwork().clear().draw();    
});