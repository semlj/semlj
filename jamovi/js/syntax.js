const syntax = {
    measures: function(vars) {
      var found = false;
      var astring="";
      var terms="";
      for(let i = 0; i < vars.length; i++) {
          if (vars[i].vars===undefined | vars[i].vars===null)
               continue;
          if (vars[i].vars.length===0)
               continue;
          found=true;     
          terms=vars[i].vars.join("+");
          astring+=vars[i].label+'=~'+terms+'\n ';
    }
    this.lav_syntax=astring;
    return(found);

    },
    make: function(eterms,latnames) {
      
      var found=false;
      var astring="";
      var terms="";
        for(let i = 0; i < latnames.length; i++) {
          if (eterms[i]===undefined | eterms[i]===null)
              continue;
          if (eterms[i].length===0)
              continue;
          found=true;
          terms=eterms[i].join("+");
          astring+=latnames[i]+'~'+terms+'\n';
    }
    this.lav_syntax+=astring;
    return(found);
    },
    varcov: function(pairs) {
      var astring="";
      var terms="";
        for(let i = 0; i < pairs.length; i++) {
          if (pairs[i].i1===null | pairs[i].i2===null)
               continue;
          astring+=pairs[i].i1+'~~'+pairs[i].i2+'\n ';
    }
    this.lav_syntax+=astring;
    },
    constraints: function(alist) {
      
      var astring="";
      for(let i = 0; i < alist.length; i++) {
          if (alist[i]!==null)
              astring+=' '+alist[i]+'  \n ';
      }
      this.lav_syntax+=astring;
    },
    clean: function() {
      this.lav_syntax="";
      
    }

};

module.exports = syntax;
