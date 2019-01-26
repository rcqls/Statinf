	// m: main, s: sim, h: hist, i: interface, f: functions
	var cqlsAEP={
				mode: "static",
				win: {top: 60, bottom: 60},
				enyo: {},
				staticValues: {},
				cycle: 0, autoPreCycle: {},autoPostCycle: {}, durations: {},
				m: {
					xylimMore:0.01,
					xmin:-5.0,xmax:5.0,ymax:0.5,qmin:0.0001,qmax:0.9999,
					nbsSim: {//to remove
						"1":[1,5,10,30,100,200,300,500,1000,1500,3000],
						"10":[10,50,100,200,300,500,1000,1500,3000],
						"30":[30,150,300,600,900,1500,3000],
						"100":[100,500,1000,1500,3000],
						"1000":[1000,3000]
					},
					nbSimMax:3000
				},
				s: {},
				h: {},
				i: {
					dim: {w:1200,h:300},
					count: 3, keepAspectRatio: 0,
					//indSim:3, indN:2,
					ptSize:3, loop:false, pause:false, anim:true , prior: false, allowLevelChange:true,
					zoom: {},
					scaleTime: 1.0
				},
				f: {}
			};

    //////////////////////////////////////////////
    // Functions
	cqlsAEP.f.resizeCanvas=function() {
		// browser viewport size
		if(typeof(cqlsAEP.i.container) != "undefined") {
			var w = cqlsAEP.i.container.width();
    	var h = w/2; //cqlsAEP.i.container.height();
		} else {
			var w = window.innerWidth;
			var h = window.innerHeight-cqlsAEP.win.top-cqlsAEP.win.bottom;
		}

		if (cqlsAEP.i.keepAspectRatio) {
		    // keep aspect ratio
		    var scale = Math.min(w / cqlsAEP.m.ow, h / cqlsAEP.m.oh);
		    cqlsAEP.m.stage.scaleX = scale;
		    cqlsAEP.m.stage.scaleY = scale;

		   	// adjust canvas size
		   	cqlsAEP.m.stage.canvas.width = cqlsAEP.m.ow * scale;
		  	cqlsAEP.m.stage.canvas.height = cqlsAEP.m.oh * scale;
		} else {
		    // scale to exact fit
		    cqlsAEP.m.stage.scaleX = w / cqlsAEP.m.ow;
		    cqlsAEP.m.stage.scaleY = h / cqlsAEP.m.oh;

		    // adjust canvas size
		    cqlsAEP.m.stage.canvas.width = cqlsAEP.m.ow * cqlsAEP.m.stage.scaleX;
		    cqlsAEP.m.stage.canvas.height = cqlsAEP.m.oh * cqlsAEP.m.stage.scaleY;
		}

		//console.log(cqlsAEP.m.stage.canvas.width+","+cqlsAEP.m.stage.canvas.height);
	    cqlsAEP.m.stage.update();
	}

	cqlsAEP.f.updateDemo=function() {
		cqlsAEP.cycle += 1;
		console.log("cycle="+cqlsAEP.cycle);
		if(cqlsAEP.autoPreCycle[cqlsAEP.cycle]) eval(cqlsAEP.autoPreCycle[cqlsAEP.cycle]);
		console.log("cqlsAEP.i.anim:"+cqlsAEP.i.anim);
		if(cqlsAEP.i.anim) {
			//cqlsAEP.f.updateSim();
			cqlsAEP.m.play.$playLongDensity();
		} else {
			//cqlsAEP.f.updateHist();
			cqlsAEP.m.play.$playShort();
		}
		if(cqlsAEP.autoPostCycle[cqlsAEP.cycle]) eval(cqlsAEP.autoPostCycle[cqlsAEP.cycle]);
	}


	cqlsAEP.f.addCount=function(nb) {
		cqlsAEP.i.count+=nb;
		if(cqlsAEP.i.count<0) cqlsAEP.i.count=0;
		if(cqlsAEP.i.count>4) cqlsAEP.i.count=4;
	}

	//MLevel correspondance: [1,3,5,10,30,100,1000,3000]
	cqlsAEP.f.setMLevel=function(lev,mode) {
		if(!mode) mode="set";
		if(mode=="add") mode="inc";
		cqlsAEP.m.play.$setMLevel(lev,mode);
	}

	cqlsAEP.f.setLevelOfHist=function(lev,hist,mode) {
		if(!mode) mode="set";
		if(mode=="add") mode="inc";
		if(hist==1) {
			histo=cqlsAEP.m.play.hist[1];
			console.log("hist[1] active")
		} else if(hist==0) {
			histo=cqlsAEP.m.play.hist[0];
		} else if(!hist || hist=="cur") {
			histo=cqlsAEP.m.play.histCur;
		}
		histo.$level(lev,mode);
	}

	cqlsAEP.f.onTap=function(x,y) {
		if(cqlsAEP.m.play.graphExp.$zoomActive()) {
			if(cqlsAEP.m.play.graphExp.$hitZoom(x,y)!="none") {
				cqlsAEP.m.play.$reset();
				cqlsAEP.m.stage.update();
			}
			return
		}
		if(y < cqlsAEP.h.plot.dim["$[]"]("y")) {

			if(cqlsAEP.i.anim) {
	    		cqlsAEP.m.play.$setMLevel(x > cqlsAEP.s.plot.dim["$[]"]("w")/2 ? 1 : -1);
	    	} else {
		    	if(x > cqlsAEP.s.plot.dim["$[]"]("w")/2) cqlsAEP.f.addCount(1); else cqlsAEP.f.addCount(-1);
	    	}
		} else {

	   		if(x > cqlsAEP.s.plot.dim["$[]"]("w")/2) cqlsAEP.m.play.histCur.$level(1); else cqlsAEP.m.play.histCur.$level(-1);
	   		//if(cqlsAEP.i.allowLevelChange) {
		    	if(!cqlsAEP.i.anim) cqlsAEP.m.play.histCur.$draw();
		    	else {
		    		if(cqlsAEP.i.paused) cqlsAEP.m.play.$drawHist();
		    	}
		    //}
		    cqlsAEP.m.stage.update();
	    }

	}

	cqlsAEP.f.tick=function(event) {
		// output
		cqlsAEP.m.outputExp.text="m="+(cqlsAEP.i.anim ?  Math.round(cqlsAEP.m.play.nbSim/cqlsAEP.m.play.n) : Math.pow(10,cqlsAEP.i.count));
		if(cqlsAEP.m.play.$transfMode()=="sample") cqlsAEP.m.outputExp.text+="\nn="+cqlsAEP.m.play.n;
		cqlsAEP.m.outputHist.text="m="+cqlsAEP.m.play.histCur.nbTot
		if(cqlsAEP.m.play.histCur.type=="cont") cqlsAEP.m.outputHist.text+="\nnbInter="+Math.pow(2,cqlsAEP.m.play.histCur.levelNext);
		//cqlsAEP.m.outputHist.text+="\n"+cqlsAEP.m.stage.mouseX+":"+cqlsAEP.m.stage.mouseY;

		cqlsAEP.m.outputExpAEP.visible= cqlsAEP.m.outputHistAEP.visible = cqlsAEP.f.getValue("checkSummary");
		if (cqlsAEP.m.outputExpAEP.visible) {
			cqlsAEP.m.outputExpAEP.text="mean="+cqlsAEP.m.play.expCur.distrib.$mean().toFixed(5);
			cqlsAEP.m.outputExpAEP.text+="\nsd="+cqlsAEP.m.play.expCur.distrib.$stdDev().toFixed(7);
			if(cqlsAEP.m.play.statMode=="ic") cqlsAEP.m.outputExpAEP.text+="\nGood IC="+((1-cqlsAEP.m.play.alpha)*100).toFixed(1)+"%";
			cqlsAEP.m.outputHistAEP.text="mean="+cqlsAEP.m.play.histCur.mean[0].toFixed(5);
			cqlsAEP.m.outputHistAEP.text+="\nsd="+cqlsAEP.m.play.histCur.sd.toFixed(7);
			if(cqlsAEP.m.play.statMode=="ic" && cqlsAEP.m.play.histCur.nbTot>0) cqlsAEP.m.outputHistAEP.text+="\nGood IC="+((cqlsAEP.m.play.histCur.cptICTot/cqlsAEP.m.play.histCur.nbTot)*100).toFixed(1)+"%";
		}

		cqlsAEP.m.stage.update(event);
	}

	//to make
	cqlsAEP.f.getValue=function(key) {
		if(cqlsAEP.mode=="enyo") {
			return cqlsAEP.enyo.app.$[key].getValue();
		} else if(cqlsAEP.mode == "static") {
			//console.log("static["+key+"]="+cqlsAEP.staticValues[key]);
			return cqlsAEP.staticValues[key];
		}
	}

	cqlsAEP.f.setValueOnly=function(key,value) {
		if(cqlsAEP.mode == "static") {
			//console.log("static["+key+"]="+cqlsAEP.staticValues[key]);
			cqlsAEP.staticValues[key]=value;
		}
	}

	cqlsAEP.f.setValue=function(key,value) {
		cqlsAEP.f.setValueOnly(key,value);
		cqlsAEP.m.play.$updateVisible();
	}

///////////////////////////////////////////////////////
// wrappers maybe to avoid if not really necessary!
	//For simulation
	cqlsAEP.f.updateStage=function() {
		cqlsAEP.cycle=0;
		cqlsAEP.m.play.$reset();
		cqlsAEP.m.stage.update();
	}

	//discreteUniform,bernoulli,binomial,birthday
	//stdNormal,uniform,normal,t,chi2,cauchy
	//saljus
	cqlsAEP.f.setDistrib=function(distrib) {
		cqlsAEP.m.play.$setDistrib(distrib);
	}

	//none,center,locationScale,square
	//sum,mean,stdMean
	//meanIC
	cqlsAEP.f.setTransf=function(transf) {
		console.log("transf:"+transf);
		cqlsAEP.m.play.$setStatMode(transf);
		if(transf=="meanIC") transf="mean";
		cqlsAEP.m.play.$setTransf(transf);
	}

	cqlsAEP.f.setN=function(n) {
		cqlsAEP.m.play.$setN(n);
	}

	cqlsAEP.f.setSimMode=function(anim,prior) {//booleans
		cqlsAEP.f.setValue('animMode',anim);
    cqlsAEP.f.setValue('priorMode',prior);
    cqlsAEP.m.play.$animMode();
	}

	cqlsAEP.f.setAlpha=function(alpha) {
		cqlsAEP.m.play.$setAlpha(alpha);
	}

	//jquery-easyui
	cqlsAEP.f.aide=function(topic) {
		$('#window-aide').window('refresh','aide/'+topic+'.html');
		$('#window-aide').window('open');
	}

	//mode static
	cqlsAEP.f.initSim=function() {
		console.log("initSim");
		//init stage parameters
		cqlsAEP.staticValues.animMode=true;
		cqlsAEP.staticValues.priorMode=false;
		cqlsAEP.staticValues.checkSummary=false;
		cqlsAEP.staticValues.checkExp0Curve=false;
		cqlsAEP.staticValues.checkExp1Curve=false;
		cqlsAEP.staticValues.checkHistCurve=false;
		cqlsAEP.staticValues.checkTCL=false;
		cqlsAEP.staticValues.checkExp0Mean=false;
		cqlsAEP.staticValues.checkExp1Mean=false;
		cqlsAEP.staticValues.checkExp0SD=false;
		cqlsAEP.staticValues.checkExp1SD=false;
		cqlsAEP.staticValues.checkHistMean=false;
		cqlsAEP.staticValues.checkHistSD=false;
		cqlsAEP.i.keepAspectRatio=false;
		cqlsAEP.i.loop=true;cqlsAEP.i.paused=false;
	}

	cqlsAEP.f.autoSim=function(scenario) {
		switch(scenario) {
			case "va":
			//prepare before or after cycle autoscript
			cqlsAEP.autoPreCycle[1]="{\
				cqlsAEP.i.scaleTime=2.0;\
				cqlsAEP.f.setMLevel(4);\
				cqlsAEP.f.setLevelOfHist(3);\
			}";
			cqlsAEP.autoPreCycle[3]="{cqlsAEP.i.scaleTime=1.0;}";
			cqlsAEP.autoPreCycle[4]="{cqlsAEP.f.setMLevel(5);cqlsAEP.f.setLevelOfHist(4);}";
			cqlsAEP.autoPreCycle[6]="{cqlsAEP.f.setValue('animMode',false);}";
			cqlsAEP.autoPreCycle[10]="{\
				cqlsAEP.f.setLevelOfHist(5);\
			 	cqlsAEP.f.addCount(3);\
			}";
			cqlsAEP.autoPreCycle[15]="{\
				cqlsAEP.f.addCount(4);\
				cqlsAEP.f.setLevelOfHist(6);\
				cqlsAEP.f.setValue('checkHistCurve',true);\
			}";
			cqlsAEP.autoPostCycle[20]="cqlsAEP.i.loop=false;";
			break;
			case "ech":
			//prepare before or after cycle autoscript
			cqlsAEP.autoPreCycle[1]="{\
				cqlsAEP.i.scaleTime=2.0;\
				cqlsAEP.f.setMLevel(4);\
				cqlsAEP.f.setLevelOfHist(3);\
			}";
			cqlsAEP.autoPreCycle[2]="{cqlsAEP.f.setValue('checkExp1Curve',true);}";
			cqlsAEP.autoPreCycle[3]="{cqlsAEP.i.scaleTime=1.0;}";
			cqlsAEP.autoPreCycle[4]="{cqlsAEP.f.setMLevel(5);cqlsAEP.f.setLevelOfHist(4);}";
			cqlsAEP.autoPreCycle[6]="{cqlsAEP.f.setValue('animMode',false);}";
			cqlsAEP.autoPreCycle[10]="{\
				cqlsAEP.f.setLevelOfHist(5);\
			 	cqlsAEP.f.addCount(3);\
			}";
			cqlsAEP.autoPreCycle[15]="{\
				cqlsAEP.f.addCount(4);\
				cqlsAEP.f.setLevelOfHist(6);\
				cqlsAEP.f.setValue('checkHistCurve',true);\
			}";
			cqlsAEP.autoPostCycle[20]="cqlsAEP.i.loop=false;";
			break;
			case "ic":
			//prepare before or after cycle autoscript
			cqlsAEP.autoPreCycle[1]="{\
					cqlsAEP.f.setMLevel(3);\
					cqlsAEP.f.setLevelOfHist(3);\
					cqlsAEP.f.setValue('checkSummary',true);\
					cqlsAEP.f.setValue('checkExp1Mean',true);\
				}";
			cqlsAEP.autoPreCycle[3]="cqlsAEP.f.setLevelOfHist(5);";
			cqlsAEP.autoPreCycle[5]="{\
				cqlsAEP.f.addCount(3);\
				cqlsAEP.f.setValue('animMode',false);\
				cqlsAEP.f.setValue('checkHistCurve',true);\
			}";
			cqlsAEP.autoPreCycle[10]="{\
				cqlsAEP.f.setValue('animMode',true);\
				cqlsAEP.f.setMLevel(5);\
			}";
			cqlsAEP.autoPreCycle[15]="{\
				cqlsAEP.f.addCount(3);\
				cqlsAEP.f.setLevelOfHist(7);\
				cqlsAEP.f.setValue('animMode',false);\
			}";
			cqlsAEP.autoPostCycle[20]="cqlsAEP.i.loop=false;";
		}
	}

	///////////////////////////
	// Main function to call
	function aepProba(scaleTextX,scaleTextY) {
		if(typeof(scaleTextX) == "undefined") scaleTextX=1.0;
		if(typeof(scaleTextY) == "undefined") scaleTextY=1.0;

	    cqlsAEP.m.canvas = document.getElementById("createjsCanvasProba");
			if(typeof(cqlsAEP.i.container) != "undefined") {
				cqlsAEP.m.ow = cqlsAEP.i.container.width();
	    	cqlsAEP.m.oh = cqlsAEP.m.ow/2;
			} else {
	    	cqlsAEP.m.ow = cqlsAEP.m.canvas.width;
				cqlsAEP.m.oh = cqlsAEP.m.canvas.height;
			}

		cqlsAEP.i.dim = {w: cqlsAEP.m.ow, h: cqlsAEP.m.oh/2};

		//Run function when browser resizes
		window.onresize=function() {
			if(typeof(cqlsHypo) != "undefined" && typeof(cqlsHypo.container) != "undefined") cqlsHypo.f.resizeCanvas();
			cqlsAEP.f.resizeCanvas();
		};

	    cqlsAEP.m.stage = new createjs.Stage(cqlsAEP.m.canvas);
	    //cqlsAEP.m.stage.autoClear = true;
	    createjs.Touch.enable(cqlsAEP.m.stage);

	    cqlsAEP.s.plot=Opal.CqlsAEP.Plot.$new();
	    cqlsAEP.m.stage.addChild(cqlsAEP.s.plot.parent);

	    cqlsAEP.h.plot=Opal.CqlsAEP.Plot.$new(Opal.hash2(["x","y","w","h"],{x:0,y:cqlsAEP.i.dim.h,w:cqlsAEP.i.dim.w,h:cqlsAEP.i.dim.h}),Opal.hash2(["bg"],{bg:"#FF8888"}));
	    cqlsAEP.m.stage.addChild(cqlsAEP.h.plot.parent);

	   	cqlsAEP.m.play=Opal.CqlsAEP.Play.$new();

	    cqlsAEP.m.play.hist[0].$level(4,"set");
	    cqlsAEP.m.play.hist[1].$level(4,"set");

	    //createjs.Ticker.setInterval(20);
		createjs.Ticker.addEventListener("tick", cqlsAEP.f.tick);

		// UI code:
		cqlsAEP.m.outputExp = cqlsAEP.m.stage.addChild(new createjs.Text("", "20px monospace", "#000"));
		cqlsAEP.m.outputExp.lineHeight = 15;
		cqlsAEP.m.outputExp.textBaseline = "top";
		cqlsAEP.m.outputExp.x = 20;
		cqlsAEP.m.outputExp.scaleX = 2*scaleTextX;cqlsAEP.m.outputExp.scaleY = 2*scaleTextY;
		//console.log("ici:"+cqlsAEP.h.plot.dim["$[]"]("y"));
		cqlsAEP.m.outputExp.y = cqlsAEP.m.outputExp.lineHeight;
		cqlsAEP.m.outputExp.alpha=0.4;

		cqlsAEP.m.outputHist = cqlsAEP.m.stage.addChild(new createjs.Text("", "20px monospace", "#000"));
		cqlsAEP.m.outputHist.lineHeight = 15;
		cqlsAEP.m.outputHist.textBaseline = "top";
		cqlsAEP.m.outputHist.x = 20;
		cqlsAEP.m.outputHist.scaleX = 2*scaleTextX;cqlsAEP.m.outputHist.scaleY = 2*scaleTextY;
		//console.log("ici:"+cqlsAEP.h.plot.dim["$[]"]("y"));
		cqlsAEP.m.outputHist.y = cqlsAEP.h.plot.dim["$[]"]("y")+cqlsAEP.m.outputHist.lineHeight;
		cqlsAEP.m.outputHist.alpha=0.4;

		cqlsAEP.m.outputExpAEP = cqlsAEP.m.stage.addChild(new createjs.Text("", "20px monospace", "#000"));
		cqlsAEP.m.outputExpAEP.lineHeight = 15;
		cqlsAEP.m.outputExpAEP.textBaseline = "top";
		cqlsAEP.m.outputExpAEP.x = cqlsAEP.i.dim["w"]- (20 + 12*25)*scaleTextX;
		cqlsAEP.m.outputExpAEP.scaleX = 2*scaleTextX; cqlsAEP.m.outputExpAEP.scaleY = 2*scaleTextY;
		//console.log("ici:"+cqlsAEP.h.plot.dim["$[]"]("y"));
		cqlsAEP.m.outputExpAEP.y = cqlsAEP.m.outputExpAEP.lineHeight;
		cqlsAEP.m.outputExpAEP.alpha=0.4;

		cqlsAEP.m.outputHistAEP = cqlsAEP.m.stage.addChild(new createjs.Text("", "20px monospace", "#000"));
		cqlsAEP.m.outputHistAEP.lineHeight = 15;
		cqlsAEP.m.outputHistAEP.textBaseline = "top";
		cqlsAEP.m.outputHistAEP.x = cqlsAEP.i.dim["w"]- (20 + 12*25)*scaleTextX;
		cqlsAEP.m.outputHistAEP.scaleX = 2*scaleTextX;cqlsAEP.m.outputHistAEP.scaleY = 2*scaleTextY;
		//console.log("ici:"+cqlsAEP.h.plot.dim["$[]"]("y"));
		cqlsAEP.m.outputHistAEP.y = cqlsAEP.h.plot.dim["$[]"]("y")+cqlsAEP.m.outputHistAEP.lineHeight;
		cqlsAEP.m.outputHistAEP.alpha=0.4;

		//Initial call
		if(typeof(cqlsAEP.i.container) != "undefined") console.log("AEP-container:"+cqlsAEP.i.container.width()+"x"+cqlsAEP.i.container.height());
		console.log("AEP (ow x oh):"+cqlsAEP.m.ow+"x"+cqlsAEP.m.oh);
		cqlsAEP.f.resizeCanvas();
		console.log("AEP:"+cqlsAEP.m.stage.canvas.width+"x"+cqlsAEP.m.stage.canvas.height);
		console.log("AEP (ow x oh) (AP):"+cqlsAEP.m.ow+"x"+cqlsAEP.m.oh);
		//if(typeof(cqlsAEP.i.container) != "undefined") console.log("hypo-container (AP):"+cqlsAEP.i.container.width()+"x"+cqlsAEP.i.container.height());
		cqlsAEP.m.outputExpAEP.x = cqlsAEP.i.dim["w"]- (20 + 12*25)*scaleTextX;

		if(cqlsAEP.mode == "enyo") {
			cqlsAEP.enyo.app.$.nMenu.hide();
			cqlsAEP.enyo.app.$.alphaMenu.hide();
			cqlsAEP.enyo.app.$.pauseButton.hide();
		}
	}
