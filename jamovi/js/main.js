
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
       console.log("multigroup type",typeof(val));
       console.log("multigroup value",JSON.stringify(val));
       
      
       


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

        this.getColumnNames = () => {
            return this.requestData('columns', {  })
				.then((data) => {
	                return data.columns.map(col => col.name);
	            }).then((names) => {
					// exclude filters
					let index = 0;
					for (;index < names.length; index++) {
						let name = names[index];
						if (/^Filter [1-9][0-9]*$/.exec(name) ||
							/^F[1-9][0-9]* \([1-9][0-9]*\)$/.exec(name))
								continue; // a filter
						else
							break; // not a filter
					}
					return names.slice(index);
				});
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

            let script = this.currentSession.getDocument().getValue();

            this.getColumnNames().then((columns) => {

              ui.view.model.options.beginEdit();

                let match = script.match(/^\s*\#\s*\((.*)\)/);
                if (match !== null) {
                    let content = match[1];
                    let vars = content.split(',');
                    vars = vars.map(s => s.trim());
                    vars = vars.filter(v => columns.includes(v));
                    ui.vars.setValue(vars);
                    ui.code.setValue(script);
					this.currentSession.allColumns = false;
                }
                else {
                    ui.vars.setValue(columns);
                    ui.code.setValue(script);
					this.currentSession.allColumns = true;
                }

                // toggle toggle so the analysis *always* reruns
                // even if nothing has changed
                ui.toggle.setValue( ! ui.toggle.value());

                ui.view.model.options.endEdit();

                this.editor.focus();
            });
    	};

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
		if ( ! this.currentSession.allColumns)
			return;
		if (event.dataType !== 'columns')
			return;
    console.log(this.getColumnNames())

		this.getColumnNames().then((columns) => {
			let old = ui.vars.value();
			if ( ! old.every((val, idx) => val === columns[idx])) {
			    console.log("adjust vars")
	  			ui.vars.setValue(columns);
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
