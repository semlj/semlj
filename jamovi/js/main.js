// Here we define the editor, which is largely taken from Rj code.
// The two issues here (as compared with Rj) is that we need to
// identify the variables in the dataset in two ways:
// one is the full list of variables, that are needed to suggest variables
// to the user.
// The second is to identify the variables used in the syntax. Please
// notice that the real parsing of variables and syntax is done in R
// which receives the syntax as a string. The parsing of observed variables
// done here is meant to fill `ui.vars` which is required for letting jamovi
// know what change in the dataset triggers the updating of the variables.

const ace = require('brace');

require('brace/mode/r');
require('brace/ext/language_tools');

require('./css');
const Suggest = require('./suggest');
const SuggestIcons = require('./suggesticons');


const events = {
    	editor_creating(ui) {

	    this.editSessions = { };

	  //  let $contents = ui.view.$el;
   	  let $contents=ui.syntax.$el;
   	  console.log("creating editor");
	    $contents.css('display', 'flex');
	    $contents.css('flex-direction', 'column');

		$contents.append(`
		    <div id="editor-box">
		        <div id="toolbar">
		            <div id="config" title="Configuration"></div>
		            <div id="run" title="Run"></div>
		        </div>
		        <div id="editor"></div>
                <div id="info">Ctrl + Shift + Enter to run</div>
            </div>`);

        let $config = $contents.find('#config');
        $config.append(`
            <div id="menu">
                <label id="fonts-label">Fonts size</label>
                <select id="fonts">
                    <option value="small">Small</option>
                    <option value="medium">Medium</option>
                    <option value="large">Large</option>
                </select>
            </div>`);

		this.$editor = $contents.find('#editor');
		this.$run = $contents.find('#run');
		this.$menu = $contents.find('#menu');

		this.$run.on('click', () => this.run(ui));

		this.$fonts = $config.find('#fonts');

		this.$fonts.on('change', (event) => {
		    var size=this.$fonts.children("option:selected").val();
		    this.$editor.css("font-size",size);
		    ui.fonts.setValue(size);
		});



		this.$menu.find('input').on('keyup', (event) => {
		    if (event.keyCode == 13)
		        this.run(ui);
		});

		$config.on('click', (event) => {
		    if (event.target === $config[0])
		        this.toggleMenu(ui);
		});

		this.$editor.on('click', () => {
            this.hideMenu(ui);
		});

		if (navigator.platform === 'MacIntel') {
		    let $info = $contents.find('#info');
		    $info.text('\u2318 + Shift + Enter to run');
		}
		


},

     loaded(ui) {
        console.log("loaded code");
       var val = ui.multigroup.value();

        this.editor = ace.edit('editor');
        this.editor.$blockScrolling = Infinity; // disable a warning
        this.editor.setShowPrintMargin(false);
        this.editor.setHighlightActiveLine(false);
        this.editor.focus();
        this.editor.setOptions({
            enableBasicAutocompletion: true,
        });
        
        
        

        this.editor.commands.on('afterExec', (event) => {
            let editor = event.editor;
            if (event.command.name == 'insertstring') {
                let position = editor.getCursorPosition();
                let line = editor.getSession().getDocument().getLine(position.row);
                let before = line.substring(0, position.column);
                if (/^[\w.]$/.test(event.args) && /[A-Za-z0-9_\.\$]{3}\:?$/.test(before))
                    editor.execCommand('startAutocomplete');
                else if (event.args == ':' && /[A-Za-z0-9]\:\:\:?$/.test(before))
                    editor.execCommand('startAutocomplete');
            }
            else if (event.command.name == 'backspace' &&
                editor.completer &&
                editor.completer.activated)
                    editor.execCommand('startAutocomplete');
            else if (event.command.name === 'startAutocomplete')
                SuggestIcons.add(event.editor);
        });

        this.editor.commands.on('exec', (event) => {
            if (event.command.name === 'indent') {
                let editor = event.editor;
                let position = editor.getCursorPosition();
                let line = editor.getSession().getDocument().getLine(position.row);
                let before = line.substring(0, position.column);
                if (/[A-Za-z0-9_\.\$\:]$/.test(before)) {
                    editor.execCommand('startAutocomplete');
                    event.preventDefault();
                }
            }
        });

        // clear the default completers
        this.editor.completers.splice(0, this.editor.completers.length);

        // add the new one
        this.editor.completers.push(new Suggest(() => {
            return this.getColumnNames();
        }));

// .getColumns() is used to gather all variables in the dataset
//  only filter variables are filtered out, the actual filtering
// for variables used by the user's syntax, use .getActiveVariables()

        this.getColumnNames = () => {
            return this.requestData('columns', {  })
				.then((data) => {
	                 return data.columns;
	            }).then((cols) => { 
 		        			// exclude filters
    	            var good=cols.filter(col => {
        	        var type = col.columnType;
				        	return(type!=="filter")
					     }).map(col => col.name);
					     return good;
	            });
        };

        this.getActiveVariables = (script) => {
            var any     =  script.split(/[,~+=:*\s]/);
            var present =  this.getColumnNames().then((cols) => 
                               cols.filter(col=> any.includes(col)));
            return(present)      
         };

        this.toggleMenu = (ui) => {
            if ( ! this.$menu.hasClass('visible'))
		        this.showMenu(ui);
		    else
		        this.hideMenu(ui);
        };

        this.showMenu = (ui) => {
		    this.$menu.addClass('visible');
        };

        this.hideMenu = (ui) => {
	        this.$menu.removeClass('visible');

	        ui.view.model.options.beginEdit();
	        ui.view.model.options.endEdit();
        };

        this.run = (ui) => {

            console.log("editor is run");
            let script = this.currentSession.getDocument().getValue();

              ui.view.model.options.beginEdit();
              ui.code.setValue(script);
              this.getActiveVariables(script).then((vars) =>{
                        vars.push(ui.multigroup.value());
                        ui.vars.setValue(vars)});
    					this.currentSession.allColumns = true;

                // toggle toggle so the analysis *always* reruns
                // even if nothing has changed
                ui.toggle.setValue( ! ui.toggle.value());

                ui.view.model.options.endEdit();

                this.editor.focus();
            }
    	

        this.$editor.on('keydown', (event) => {

            if (event.keyCode === 13 && (event.metaKey || event.ctrlKey) && event.shiftKey) {
                // ctrl+shift+enter
                this.run(ui);
                event.stopPropagation();
            }
            else if (event.keyCode === 65 && event.metaKey) {
                // ctrl+a
                this.$editor.select();
            }
            else if (event.keyCode === 67 && (event.metaKey || event.ctrlKey) && event.shiftKey)             {
                // ctrl+shift+c
                this.editor.toggleCommentLines();
            }
            else if (event.keyCode === 191 && (event.metaKey || event.ctrlKey)) {
                // ctrl+/
                this.editor.toggleCommentLines();
            }
        });


	},


	onDataChanged(ui, event) {

    return;
    // this was needed in previos versions
    // let's keep it for future referece
    
		if ( ! this.currentSession.allColumns)
			return;
		if (event.dataType !== 'columns')
			return;

      
		this.getColumnNames().then((columns) => {
			let old = ui.vars.value();
			if (JSON.stringify(columns) !== JSON.stringify(old)) {
//	  			ui.vars.setValue(columns);
			}
		});

	},

    update(ui, event) {
        console.log("update");
        let id = event.id;
        this.currentSession = this.editSessions[id];

        if (this.currentSession === undefined) {

            let code = ui.code.value();
            this.currentSession = ace.createEditSession(code, 'ace/mode/r');
            this.editSessions[id] = this.currentSession;
        }
        this.$editor.css("font-size",ui.fonts.value());
        this.$fonts.val(ui.fonts.value());

        this.editor.setSession(this.currentSession);
    },
};


module.exports = events;
