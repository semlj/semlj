const DEBUG=true;

const syntax = require('./syntax');
const events = {
    update: function(ui) {
      console.log("general update");
      updateModelLabels(ui,this);
      calcCustomVariables(ui,this);
      updateSuppliers(ui,this);
      cleanEndogenousTerms(ui,this);
      cleanSecondOrder(ui,this);
      cleanEsem(ui,this);
    
      update_syntax(ui,this); 

    },

    onChange_items_changed: function(ui) {
    console.log("items changed");
      updateModelLabels(ui,this);
      updateSuppliers(ui,this);
      cleanEndogenousTerms(ui,this);
      cleanSecondOrder(ui,this);
      cleanEsem(ui,this);
      update_syntax(ui,this);
    },

    onChange_latentName: function(ui) {
     console.log("change latent name");
     updateSuppliers(ui,this);
     cleanEndogenousTerms(ui,this);
     cleanSecondOrder(ui,this);
     cleanEsem(ui,this);
     update_syntax(ui,this);

    },
    onChange_SecondName: function(ui) {
     console.log("change second name");
     updateSuppliers(ui,this);
     cleanEndogenousTerms(ui,this);
     cleanSecondOrder(ui,this);
     update_syntax(ui,this);

    },

     onChange_endogenousTerms: function(ui) {
       console.log(" endogenousTerms changed"); 
       cleanSelfEndogenous(ui,this);
       update_syntax(ui,this);

    },
    onChange_endogenousSupplier: function(ui) {
     console.log("endosupplier change");

    },
    onUpdate_endogenousSupplier: function(ui) {
     console.log("endosupplier update");

    },

    onChange_secondorderSupplier: function(ui) {
     console.log("secondorder supplier change");

    },
    onUpdate_secondorderSupplier: function(ui) {
     console.log("second order supplier update");
    },
    onChange_secondorderTerms: function(ui) {
       console.log(" secondorderTerms changed"); 
       cleanSelfSecondorder(ui,this);
       update_syntax(ui,this);

    },

    onChange_varcov: function(ui) {

      update_syntax(ui,this);
    },
 
    onChange_varcovSupplier: function(ui) {
        let values = this.itemsToValues(ui.varcovSupplier.value());
        this.checkPairsValue(ui.varcov, values);
      
    },
    onUpdate_varcovSupplier: function(ui) {
      console.log("varcovsup update");
      
    },
    
    onChange_constraints: function(ui) {
      console.log("constraints changed");
      update_syntax(ui,this);
      
      
    },

     onChange_multigroup: function(ui) {
       
       var val = ui.multigroup.value();
       if (typeof(val)==="string")
             if (val.replace(/ /g,'').length===0)
                ui.multigroup.setValue(null);
             else {
               var vars=ui.vars.value();
               let newvars=vars.map((x) => x);
                   if (!newvars.includes(val)) newvars.push(val);
                   ui.vars.setValue(newvars);
             }

     },
      onChange_cluster: function(ui) {
       console.log("Multilevel cluster changed");
       var val = ui.cluster.value();
       if (typeof(val)==="string")
             if (val.replace(/ /g,'').length===0)
                ui.cluster.setValue(null);
        else {
               var vars=ui.vars.value();
               let newvars=vars.map((x) => x);
                   if (!newvars.includes(val)) newvars.push(val);
                   ui.vars.setValue(newvars);
             }        
     },

     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    }



};



var initializeAll = function(ui, context) {
    console.log("initialize all");
//    updateSuppliers(ui,context);
//    prepareEndogenousTerms(ui,context);

};




const calcCustomVariables=function(ui,context) {
  
  console.log("calcCustomVariables");
  var endogenousIndicators=context.cloneArray(ui.endogenous.value(),[]);
  var exogenousIndicators=context.cloneArray(ui.exogenous.value(),[]);
  var secondorderIndicators=context.cloneArray(ui.secondorder.value(),[]);

  var endogenous=getLabels(endogenousIndicators);
  var exogenous=getLabels(exogenousIndicators);
  var secondorder=getLabels(secondorderIndicators);

  var latent=endogenous.concat(exogenous);
      latent=latent.concat(secondorder);

  let customVariables = [];
    for(let i = 0; i < latent.length; i++) {
        customVariables.push( { name: latent[i], measureType: 'none', dataType: 'none', levels: [] } );
    }
  context.setCustomVariables(customVariables);

  
};

const updateSuppliers=function(ui, context) {
  
  console.log("updateSuppliers function");
  var endogenousIndicators=context.cloneArray(ui.endogenous.value(),[]);
  var exogenousIndicators=context.cloneArray(ui.exogenous.value(),[]);
  var secondorderIndicators=context.cloneArray(ui.secondorder.value(),[]);

  var endogenous=getLabels(endogenousIndicators);
  var exogenous=getLabels(exogenousIndicators);
  var secondorder=getLabels(secondorderIndicators);

  var latent=endogenous.concat(exogenous);

 // esem accept only  first order factors
    ui.esemSupplier.setValue(context.valuesToItems(latent, FormatDef.variable));

 // the other suppliers need also the second order factors  
    latent=latent.concat(secondorder);

  let customVariables = [];
    for(let i = 0; i < latent.length; i++) {
        customVariables.push( { name: latent[i], measureType: 'none', dataType: 'none', levels: [] } );
    }

  context.setCustomVariables(customVariables);
  ui.endogenousSupplier.setValue(context.valuesToItems(latent, FormatDef.variable));
  ui.secondorderSupplier.setValue(context.valuesToItems(latent, FormatDef.variable));

  var vars=[];  
   for (var i = 0; i < endogenousIndicators.length; i++) {
      if  (endogenousIndicators[i]!==undefined)
        if  (endogenousIndicators[i].vars!==undefined & endogenousIndicators[i].vars!==null) {
             for (var j = 0; j < endogenousIndicators[i].vars.length; j++) {
                vars.push(endogenousIndicators[i].vars[j]); 
             }
        }
     }
   for (var i = 0; i < exogenousIndicators.length; i++) {
      if  (exogenousIndicators[i]!==undefined)
        if  (exogenousIndicators[i].vars!==undefined & exogenousIndicators[i].vars!==null) {
             for (var j = 0; j < exogenousIndicators[i].vars.length; j++) {
                vars.push(exogenousIndicators[i].vars[j]); 
             }
        }
     }

  var allvars=unique(latent.concat(vars));
  ui.varcovSupplier.setValue(context.valuesToItems(allvars, FormatDef.variable));
  context.workspace.varcovSupplierList=allvars;

};

const updateModelLabels = function(ui,context) {
  
    var list1 = ui.endogenous.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue("Endogenous" + (index + 1) );
    });
    var list2 = ui.exogenous.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue("Exogenous" + (index + 1) );
    });
    
    var list3 = ui.secondorder.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue("Factor " + (index + 1) );
    });
    

};

var prepareEndogenousTerms= function(ui,context) {

     // here we prepare a list of lists, one of each endogenous variable.
     // we also want to put a label to show which dependent variable the user is working on

     console.log("prepareEndogenousTerms");
     var endogenous = getLabels(context.cloneArray(ui.endogenous.value(),[]));
     var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);
     var secondorder = getLabels(context.cloneArray(ui.secondorder.value(),[]));
     endogenous = endogenous.concat(secondorder);
     
 
     // we make sure that there are enough arrays in the array list, each for each endogeneous
     var okList= [];
     for (var i = 0; i < endogenous.length; i++) {
         var aList = endogenousTerms[i] === undefined ? [] : endogenousTerms[i] ;
             okList.push(aList);
     }
      ui.endogenousTerms.setValue(okList);    

     // we give a label for each endogenous model
     labelize(ui.endogenousTerms, endogenous, "Endogenous");
  
     console.log("....end");
};


var cleanEndogenousTerms= function(ui,context) {

    console.log("cleanEndogenousTerms");
    prepareEndogenousTerms(ui,context);
    var endogenous  = getLabels(context.cloneArray(ui.endogenous.value(), []));
    var secondorder = getLabels(context.cloneArray(ui.secondorder.value(), []));
        endogenous  = endogenous.concat(secondorder);
        
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++) {
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);
    }

    var endogenousSupplierList = context.cloneArray(context.itemsToValues(ui.endogenousSupplier.value()),[]);

    var diff = context.findChanges("endogenousSupplierList",endogenousSupplierList,context);
    
    if (diff.hasChanged) {
      for (var i = 0; i < endogenousTerms.length; i++) 
           for (var j = 0; j < diff.removed.length; j++) {
                endogenousTerms[i]=removeFromList(diff.removed[j],endogenousTerms[i],context,1);
           }
    }
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);
    

};

var cleanSelfEndogenous=function(ui,context) {
  
    var endogenous  = getLabels(context.cloneArray(ui.endogenous.value(), []));
    var secondorder = getLabels(context.cloneArray(ui.secondorder.value(), []));
        endogenous  = endogenous.concat(secondorder);
        
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++) {
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);
    }
  
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);
  
  
}


var cleanSecondOrder= function(ui,context) {

    console.log("cleanSecondOrder");
    var secondorderTerms = context.cloneArray(ui.secondorder.value(),[]);

    var secondorderSupplierList = context.cloneArray(context.itemsToValues(ui.secondorderSupplier.value()),[]);
    var diff = context.findChanges("secondorderSupplierList",secondorderSupplierList,context);

    if (diff.hasChanged) {
    var vars=[];    
      for (var i = 0; i < secondorderTerms.length; i++) 
            for (var j = 0; j < diff.removed.length; j++) {
//                secondorderTerms[i].vars=removeFromList(diff.removed[j],secondorderTerms[i].vars,context,1);
                if (secondorderTerms[i].vars!==null) {
                    vars=removeFromList(diff.removed[j],secondorderTerms[i].vars,context,1);
                    // the remove function was written for arrays of array. Here we need
                    // arrays of string, so we fixed brutaly. Needs work.
                    vars=vars.map(function(e) { return e.join("")});
                    secondorderTerms[i].vars=vars;
                }

            }
    }

    ui.secondorder.setValue(secondorderTerms);
};


var cleanSelfSecondorder=function(ui,context) {
  
    var secondorderTerms = context.cloneArray(ui.secondorder.value(), []);
    var secondorder=getLabels(secondorderTerms);
    var vars=[];

    for (var i = 0; i < secondorder.length; i++) {
            if (secondorderTerms[i].vars!==null) {
                    vars=removeFromList(secondorder[i],secondorderTerms[i].vars,context,1);
                    // the remove function was written for arrays of array. Here we need
                    // arrays of string, so we fixed brutaly. Needs work.
                    vars=vars.map(function(e) { return e.join("")});
                    secondorderTerms[i].vars=vars;
       }
     }
  
    ui.secondorder.setValue(secondorderTerms);

}

var cleanEsem= function(ui,context) {

    console.log("cleanEsem");
    var esemTerms = context.cloneArray(ui.esem_terms.value(),[]);
    console.log(esemTerms);
    var esemSupplierList = context.cloneArray(context.itemsToValues(ui.esemSupplier.value()),[]);
    var diff = context.findChanges("esemSupplierList",esemSupplierList,context);
    console.log(diff);

    if (diff.hasChanged) {
      
      var esemTerms = esemTerms.map(x => {
            var n=x.filter(z => { return(!diff.removed.includes(z))} )
            return(n)
            });
    }

    ui.esem_terms.setValue(esemTerms);
};

var update_syntax=function(ui,context) {
  

    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);
    var secondorder=context.cloneArray(ui.secondorder.value(),[]);
    // second order factors can be handle like an endogenous variable
    var endogenous=context.cloneArray(ui.endogenous.value(),[]);
        endogenous=endogenous.concat(secondorder);
    
    var exogenous=context.cloneArray(ui.exogenous.value(),[]);
    var vars=exogenous.concat(endogenous);

    var varcov=context.cloneArray(ui.varcov.value(),[]);
    var constraints=context.cloneArray(ui.constraints.value(),[]);
    var is_measures= syntax.measures(vars);
    var is_terms =syntax.make(endogenousTerms,getLabels(endogenous));
    syntax.varcov(varcov);
    syntax.constraints(constraints);
    
    if (is_measures || is_terms )
             ui.code.setValue(syntax.lav_syntax);
    else    
             ui.code.setValue("")
    

};

var getLabels=function(alist,context) {
     var okList=[];
     for (var i = 0; i < alist.length; i++) {
         if (alist[i].label!==undefined) {
           if (alist[i].vars===undefined | alist[i].vars===null)
               continue;
           if (alist[i].vars.length===0)
               continue;

           okList.push(alist[i].label);
      }
     }
     return(okList);
};

var unique=function(arr) {
    var u = {}, a = [];
    for(var i = 0, l = arr.length; i < l; ++i){
        var prop=ssort(JSON.stringify(arr[i]));
        if(!u.hasOwnProperty(prop) && arr[i].length>0) {
            a.push(arr[i]);
            u[prop] = 1;
        }
    }
    return a;
};

var ssort= function(str){
  str = str.replace(/[`\[\]"\\\/]/gi, '');
  var arr = str.split(',');
  var sorted = arr.sort();
  return sorted.join('');
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

