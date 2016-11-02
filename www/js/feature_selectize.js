$(document).on("shiny:bound", function(event) { 
  if (event.target.id === feature_unique_listing) { 
    var selector = $(document.getElementById(feature_unique_listing).firstChild.lastElementChild.firstElementChild);
    var selectize = $(document.getElementById(feature_unique_listing).firstChild.lastElementChild.firstElementChild)[0].selectize; 
    if ($(document.getElementById(feature_all)).is(":checked")) {
      selectize.disable();
    }
    $(document.getElementById(feature_all)).click( function(){
      if( $(this).is(':checked') ) {
        selectize.disable();
      }
      else {
        selectize.enable();
      }
    });
    selectize.$control_input.on("keydown", function(e) { 
      if (e.keyCode === 13) { 
        var values = selectize.$control_input.val().split(","); 
        var notFound = ""; 
        for (var index in values) { 
          value = values[index]; 
          if (selectize.options[value] !== undefined) { 
            selectize.addItem(value); 
          } 
          else { 
            if ((value.indexOf(":") > -1) || (value.indexOf("-") > -1)) { 
              var regexp = /^([^\d:-]+)(\d+)[:-](\d+)/; 
              var new_values = regexp.exec(value).slice(1); 
              for (i=new_values[1];i<=new_values[2];i++) { 
                tmp = new_values[0] + i; 
                if (selectize.options[tmp] !== undefined) { 
                  selectize.addItem(tmp); 
                }
                else { 
                  notFound += tmp + " "; 
                } 
              } 
            } 
            else { 
              notFound += value + " "; 
            } 
          } 
        } 
        if (notFound !== "") { 
          alert(notFound + "not found."); 
        } 
        selectize.$control_input.val(""); 
      } 
    });
  }
});
