const DEBUG=true;

const syntax = require('./syntax');
const events = {
    update: function(ui) {
      console.log("general update");
      updateModelLabels(ui,this);
      updateSuppliers(ui,this);
      prepareEndogenousTerms(ui,this);
      cleanEndogenousTerms(ui,this);
      update_syntax(ui,this);
    },

    onChange_endo_items_added: function(ui) {
    console.log("endo items added");
      updateModelLabels(ui,this);
      updateSuppliers(ui,this);
      prepareEndogenousTerms(ui,this);
      update_syntax(ui,this);

    },
    onChange_endo_items_removed: function(ui) {
      console.log("endo items remove");
      updateSuppliers(ui,this);
      prepareEndogenousTerms(ui,this);
      update_syntax(ui,this);

    },
    onChange_endo_items_changed: function(ui) {
      console.log("endo items changed");
      update_syntax(ui,this);

    },

    onChange_exo_items_added: function(ui) {
    console.log("endo items added");
      updateModelLabels(ui,this);
      updateSuppliers(ui,this);
      prepareEndogenousTerms(ui,this);
      update_syntax(ui,this);

    },
    onChange_exo_items_removed: function(ui) {
      console.log("endo items remove");
      updateSuppliers(ui,this);
      prepareEndogenousTerms(ui,this);
      update_syntax(ui,this);

    },
    onChange_exo_items_changed: function(ui) {
      console.log("endo items changed");
      update_syntax(ui,this);

//      prepareEndogenousTerms(ui,this);
    },


    onChange_endogenousSupplier: function(ui) {
       console.log("endogenousSupplier has changed");
//       console.log("nothing done");


    },

    onUpdate_endogenousSupplier: function(ui) {
            console.log("endogenousSupplier update");

    },
    
    onChange_latentName: function(ui) {
     console.log("change latent name");
      prepareEndogenousTerms(ui,this);
      updateSuppliers(ui,this);
      update_syntax(ui,this);

    },

     onChange_endogenousTerms: function(ui) {
       console.log(" endogenousTerms changed"); 
      cleanEndogenousTerms(ui,this);
      update_syntax(ui,this);

    },


     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    },

     onChange_nothing: function(ui) {
      console.log("I did not do anything");
    }
  

};



var initializeAll = function(ui, context) {
    console.log("initialize all");
//    updateSuppliers(ui,context);
//    prepareEndogenousTerms(ui,context);

};





const updateSuppliers=function(ui, context) {
  
  console.log("updateSuppliers function");
  var endogenous=getLabels(context.cloneArray(ui.endogenous.value(),[]));
  var exogenous=getLabels(context.cloneArray(ui.exogenous.value(),[]));
  var latent=endogenous.concat(exogenous);

  let customVariables = [];
    for(let i = 0; i < latent.length; i++) {
        customVariables.push( { name: latent[i], measureType: 'none', dataType: 'none', levels: [] } );
    }
  
  context.setCustomVariables(customVariables);
  ui.endogenousSupplier.setValue(context.valuesToItems(latent, FormatDef.variable));
  
};

const updateModelLabels = function(ui,context) {
    var list1 = ui.endogenous.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue("Endogenous " + (index + 1) );
    });
    var list2 = ui.exogenous.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue("Exogenous " + (index + 1) );
    });
    
    

};

var prepareEndogenousTerms= function(ui,context) {

     // here we prepare a list of lists, one of each endogenous variable.
     // we also want to put a label to show which dependent variable the user is working on

     console.log("prepareEndogenousTerms");
     
     var endogenous = getLabels(context.cloneArray(ui.endogenous.value(),[]));
     var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);
     
 
     // we make sure that there are enough arrays in the array list, each for each endogeneous
     var okList= [];
     for (var i = 0; i < endogenous.length; i++) {
         var aList = endogenousTerms[i] === undefined ? [] : endogenousTerms[i] ;
             okList.push(aList);
     }
      console.log(context.customVariables);
      console.log(endogenousTerms);

      ui.endogenousTerms.setValue(okList);    
      
     // we give a label for each endogenous model
     labelize(ui.endogenousTerms, endogenous, "Endogenous");
  
  
};


var cleanEndogenousTerms= function(ui,context) {

    console.log("cleanEndogenousTerms");
    var endogenous = getLabels(context.cloneArray(ui.endogenous.value(), []));
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++)
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);


    var endogenousSupplierList = context.cloneArray(context.itemsToValues(ui.endogenousSupplier.value()),[]);
    var diff = context.findChanges("endogenousSupplierList",endogenousSupplierList,context);
    if (diff.hasChanged) {
      for (var i = 0; i < endogenous.length; i++) 
           for (var j = 0; j < diff.removed.length; j++) {
                endogenousTerms[i]=removeFromList(diff.removed[j],endogenousTerms[i],context,1);
           }
           
    }
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);
    
    

};

var update_syntax=function(ui,context) {
  

    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);
    var endogenous=context.cloneArray(ui.endogenous.value(),[]);
    var exogenous=context.cloneArray(ui.exogenous.value(),[]);
    var vars=endogenous.concat(exogenous);

    syntax.measures(vars);
    syntax.make(endogenousTerms,getLabels(endogenous));
    console.log(syntax.lav_syntax);
    ui.code.setValue(syntax.lav_syntax);

  
};

var getLabels=function(alist,context) {
     var okList=[];
     for (var i = 0; i < alist.length; i++) {
         if (alist[i].label!==undefined)
                       okList.push(alist[i].label);
     }
     return(okList);
};


var labelize = function(widget, labels, prefix) {
     widget.applyToItems(0, (item, index) => {
           item.controls[0].setPropertyValue("label",prefix +" = "+labels[index]);
        });
};


// remove a list or a item from list
// order=0 remove only if term and target term are equal
// order>0 remove if term length>=order 
// for instance, order=1 remove any matching interaction with terms, keeps main effects
// order=2 remove from 3-way interaction on (keep up to 2-way interactions)

var removeFromList = function(quantum, cosmos, context, order = 1) {

    cosmos = normalize(context.cloneArray(cosmos));
    quantum = normalize(context.cloneArray(quantum));

 
     if (cosmos===undefined)
        return([]);
     cosmos = context.cloneArray(cosmos);
       for (var i = 0; i < cosmos.length; i++) {
          if (cosmos[i]===undefined)
             break;
          var aCosmos = context.cloneArray(cosmos[i]);
           for (var k = 0; k < quantum.length; k++) {
             var  test = order === 0 ? FormatDef.term.isEqual(aCosmos,quantum[k]) : FormatDef.term.contains(aCosmos,quantum[k]);
                 if (test && (aCosmos.length >= order)) {
                        cosmos.splice(i, 1);
                        i -= 1;
                    break;    
                    }
          }
            
       }
  
    return(cosmos);
};



var storeComponent = function(id,cosmos,context) {
                    context.workspace[id]=cosmos;
};


var normalize = function(cosmos) {

  if (cosmos===undefined)
          return [];
  if (dim(cosmos)===0)
          cosmos=[cosmos];
          
        for (var i = 0; i < cosmos.length; i++) {
            var aValue = cosmos[i];
            var newValue=dim(aValue)>0 ? aValue : [aValue];
            cosmos[i]=newValue;
        }
        return cosmos;
};

var dim = function(aList) {

    if (!Array.isArray(aList))
           return(0);
    if (!Array.isArray(aList[0]))
           return(1);
    if (!Array.isArray(aList[0][0]))
           return(2);
    if (!Array.isArray(aList[0][0][0]))
           return(3);
    if (!Array.isArray(aList[0][0][0][0]))
           return(4);

  
    return(value);
};



module.exports = events;

