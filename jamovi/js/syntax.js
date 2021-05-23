const syntax = {
    measures: function(vars) {
      var astring="";
      var terms="";
        for(let i = 0; i < vars.length; i++) {
          if (vars[i].vars===undefined | vars[i].vars===null)
               continue;
          terms=vars[i].vars.join("+");
          astring+=vars[i].label+"=~"+terms+"; ";
    }
    this.lav_syntax=astring;

    },
    make: function(eterms,latnames) {
      var astring="";
      var terms="";
        for(let i = 0; i < latnames.length; i++) {
          terms=eterms[i].join("+");
          astring+=latnames[i]+"~"+terms+"; ";
    }
    this.lav_syntax+=astring;
    }
};

module.exports = syntax;
