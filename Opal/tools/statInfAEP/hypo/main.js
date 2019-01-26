	// m: main, s: sim, h: hist, i: interface, f: functions
	var cqlsHypo={
				mode: "static",
				enyo: {},
				win: {top: 60, bottom:60},
				staticValues: {},
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
	cqlsHypo.f.resizeCanvas=function() {
		// browser viewport size
		if(typeof(cqlsHypo.i.container) != "undefined") {
			var w = cqlsHypo.i.container.width();
    	var h = w/2; //cqlsHypo.i.container.height();
		} else {
			var w = window.innerWidth;
			var h = window.innerHeight-cqlsHypo.win.top-cqlsHypo.win.bottom;
		}


		if (cqlsHypo.i.keepAspectRatio) {
		    // keep aspect ratio
		    var scale = Math.min(w / cqlsHypo.m.ow, h / cqlsHypo.m.oh);
		    cqlsHypo.m.stage.scaleX = scale;
		    cqlsHypo.m.stage.scaleY = scale;

		   // adjust canvas size
		   	cqlsHypo.m.stage.canvas.width = cqlsHypo.m.ow * scale;
		  	cqlsHypo.m.stage.canvas.height = cqlsHypo.m.oh * scale;
		} else {
		    // scale to exact fit
		    cqlsHypo.m.stage.scaleX = w / cqlsHypo.m.ow;
		    cqlsHypo.m.stage.scaleY = h / cqlsHypo.m.oh;

		    // adjust canvas size
		    cqlsHypo.m.stage.canvas.width = cqlsHypo.m.ow * cqlsHypo.m.stage.scaleX;
		    cqlsHypo.m.stage.canvas.height = cqlsHypo.m.oh * cqlsHypo.m.stage.scaleY;
		}

		//console.log(cqlsHypo.m.stage.canvas.width+","+cqlsHypo.m.stage.canvas.height);
	    cqlsHypo.m.stage.update();
	}

	cqlsHypo.f.onTap=function(x,y) {
		if(cqlsHypo.m.play.graphExp.$zoomActive()) {
			if(cqlsHypo.m.play.graphExp.$hitZoom(x,y)!="none") {
				cqlsHypo.m.play.$reset();
				cqlsHypo.m.stage.update();
			}
			return
		}
		if(y < cqlsHypo.h.plot.dim["$[]"]("y")) {

			if(cqlsHypo.i.anim) {
	    		cqlsHypo.m.play.$setMLevel(x > cqlsHypo.s.plot.dim["$[]"]("w")/2 ? 1 : -1);
	    	} else {
		    	if(x > cqlsHypo.s.plot.dim["$[]"]("w")/2) cqlsHypo.i.count+=1; else cqlsHypo.i.count -=1;
		    	if(cqlsHypo.i.count<0) cqlsHypo.i.count=0;
		    	if(cqlsHypo.i.count>4) cqlsHypo.i.count=4;
	    	}
		} else {

	   		if(x > cqlsHypo.s.plot.dim["$[]"]("w")/2) cqlsHypo.m.play.histCur.$level(1); else cqlsHypo.m.play.histCur.$level(-1);
	   		//if(cqlsHypo.i.allowLevelChange) {
		    	if(!cqlsHypo.i.anim) cqlsHypo.m.play.histCur.$draw();
		    	else {
		    		if(cqlsHypo.i.paused) cqlsHypo.m.play.$drawHist();
		    	}
		    //}
		    cqlsHypo.m.stage.update();
	    }

	}

	cqlsHypo.f.getValue=function(key) {
		if(cqlsHypo.mode=="enyo") {
			if (key=="param" || key == "side") {
				//console.log("key:" + key + "," + cqlsHypo.enyo.app.$[key+"MenuName"].getContent());
				return cqlsHypo.enyo.app.$[key+"MenuName"].getContent();
			} else {
				return cqlsHypo.enyo.app.$[key].getValue();
			}
		} else if(cqlsHypo.mode == "static") {
			//console.log("static["+key+"]="+cqlsHypo.staticValues[key]);
			return cqlsHypo.staticValues[key];
		}
	}

	cqlsHypo.f.setValue=function(key,value) {
		if(cqlsHypo.mode == "static") {
			cqlsHypo.staticValues[key]=value;
		}
	}

	cqlsHypo.f.setValueWithReset=function(key,value) {
	 	cqlsHypo.f.setValue(key,value);
		cqlsHypo.m.play.$reset();
	}



	//mode static
	cqlsHypo.f.initHypo=function(param,side,refValue,nValue,meanValue,sdValue) {
		console.log("initHypo");
		//init stage parameters
		cqlsHypo.f.setValue('param',param);
		cqlsHypo.f.setValue('side',side);
		cqlsHypo.f.setValue('refValue',refValue);
		cqlsHypo.f.setValue('nValue',nValue);
		cqlsHypo.f.setValue('meanValue',meanValue);
		cqlsHypo.f.setValue('sdValue',sdValue);

		cqlsHypo.f.setValue('checkParam0Curve',false);
		cqlsHypo.f.setValue('checkParam1Curve',false);
		cqlsHypo.f.setValue('checkDelta0Curve',false);
		cqlsHypo.f.setValue('checkDelta1Curve',false);

		cqlsHypo.f.setValue('checkParamLim',false);
		cqlsHypo.f.setValue('checkDeltaLim',false);
		cqlsHypo.f.setValue('checkData',false);
		cqlsHypo.f.setValue('checkRiskTypeI',false);

		cqlsHypo.f.setValue('checkRiskTypeGen',false);
		cqlsHypo.f.setValue('checkPval',false);
		cqlsHypo.f.setValue('checkParam0Mean',false);
		cqlsHypo.f.setValue('checkParam0SD',false);
		cqlsHypo.f.setValue('checkParam1Mean',false);
		cqlsHypo.f.setValue('checkParam1SD',false);
		cqlsHypo.f.setValue('checkDelta0Mean',false);
		cqlsHypo.f.setValue('checkDelta0SD',false);
		cqlsHypo.f.setValue('checkDelta1Mean',false);
		cqlsHypo.f.setValue('checkDelta1SD',false);
	}

	cqlsHypo.f.easyuiCheckSelect=function(selected) {
		var e;
		for (i = 0; i < selected.length; i++) {
			e='check'+selected[i];
			//console.log(e);
			cqlsHypo.f.setValue(e,true);
			$('#hypo-'+e).linkbutton('select');
		}
	}

	///////////////////////////
	// Main function to call
	function aepHypo(scaleTextX,scaleTextY) {

		if(typeof(scaleTextX) == "undefined") scaleTextX=1.0;
		if(typeof(scaleTextY) == "undefined") scaleTextY=1.0;

		// console.log(Opal.CqlsHypo.$range(0,1,.1));
		// console.log(Opal.CqlsHypo.$seq(0,1,11));
		// console.log(jStat.seq(0,1,11));

		//cqlsHypo.d=Opal.CqlsHypo.Timing["$[]"](10,20,14);
		// cqlsHypo.d=Opal.CqlsHypo.Timing.$new([10,20,12]);
		// console.log(cqlsHypo.d.t);
		// console.log(cqlsHypo.d.d);
		// console.log(cqlsHypo.d.$start());
		// console.log(cqlsHypo.d.$stop());

		////// test on Distribution
		// cqlsHypo.m.exp=Opal.CqlsHypo.Distribution.$new();
		// cqlsHypo.m.exp.$set("binomial",[2,.5]);
		// console.log(cqlsHypo.m.exp.$pdf([-1,1,1.15]));
		// cqlsHypo.m.dist=Opal.CqlsHypo.Convolution.$power(cqlsHypo.m.exp,3);

		//cqlsHypo.m.dist=new BinomialDistribution(5,.15);
		// cqlsHypo.m.dist=new LocationScaleDistribution(new BernoulliDistribution(.15),-.15/Math.sqrt(.15*.85),1/Math.sqrt(.15*.85));
		// console.log(cqlsHypo.m.dist.minValue());
		// console.log(cqlsHypo.m.dist.maxValue());
		// console.log(cqlsHypo.m.dist.type()==CONT);
		// console.log(cqlsHypo.m.dist.step());
		// console.log(cqlsHypo.m.dist.values());
		// cqlsHypo.m.dist2 = new PowerDistribution(cqlsHypo.m.dist,2);
		// console.log(cqlsHypo.m.dist2.minValue());
		// console.log(cqlsHypo.m.dist2.maxValue());
		// console.log(cqlsHypo.m.dist2.type()==CONT);
		// console.log(cqlsHypo.m.dist2.step());
		// console.log(cqlsHypo.m.dist2.values());

		// cqlsHypo.m.dist3=Opal.CqlsHypo.Convolution.$power(cqlsHypo.m.dist2,4);
		// console.log(cqlsHypo.m.dist3.values());
		// console.log(cqlsHypo.m.dist3.values().map(cqlsHypo.m.dist3.density));
		// console.log(cqlsHypo.m.dist3.values().map(cqlsHypo.m.dist3.density).reduce(function(a, b) {
		//     return a + b;
		// }));
				// d2=Distribution.new
				// d2.setAsTransfOf(d,{name: :square, args: []})

		// cqlsHypo.m.exp2=Opal.CqlsHypo.Distribution.$new();
		// cqlsHypo.m.exp2.$setAsTransfOf(cqlsHypo.m.exp,Opal.hash2(["name","args"],{name: "square",args: [2]}));
		// cqlsHypo.m.dist=new LocationScaleDistribution(new Convolution(new UniformDistribution(0,1),2),0,.5);
		// console.log(cqlsHypo.m.dist.minValue());
		// console.log(cqlsHypo.m.dist.maxValue());
		// console.log(cqlsHypo.m.dist.type()==CONT);
		// console.log(cqlsHypo.m.dist.step());

		// console.log(cqlsHypo.m.dist.density(0));
		// console.log(cqlsHypo.m.dist.density(.5));
		// console.log(cqlsHypo.m.dist.density(1));
		// console.log(cqlsHypo.m.dist.density(2));
		// console.log(cqlsHypo.m.dist.density(3));
		// console.log(cqlsHypo.m.dist.density(4));
		// console.log(cqlsHypo.m.dist.density(5));
		// console.log(cqlsHypo.m.dist.density(6));
		// console.log(cqlsHypo.m.dist.density(7));
		// console.log(cqlsHypo.m.dist.density(8));
		// console.log(cqlsHypo.m.dist.density(9));

		// cqlsHypo.m.exp2.$set("binomial",[8,.5]);
		// console.log(cqlsHypo.m.exp2.$pdf([0,1,2,3,4]));
		// console.log(cqlsHypo.m.exp2.$quantile(0));
		// console.log(cqlsHypo.m.exp2.$quantile(1));
		// console.log(cqlsHypo.m.exp2.$minValue());
		// console.log(cqlsHypo.m.exp2.$maxValue());
		// console.log(cqlsHypo.m.exp2.$pdf([0,1,2,3,4]));
		// console.log(cqlsHypo.m.exp2.distrib.step());

		// cqlsHypo.m.exp.$set("binomial",[1,0.5]);
		// cqlsHypo.m.exp3=Opal.CqlsHypo.Distribution.$new();
		// cqlsHypo.m.exp3.$set("binomial",[10,0.5]);
		// cqlsHypo.m.exp2=Opal.CqlsHypo.Distribution.$new();
		// cqlsHypo.m.exp2.$setAsTransfOf(cqlsHypo.m.exp,Opal.hash2(["name","args"],{name: "mean",args: [10]}));
		// console.log(cqlsHypo.m.exp2.distrib.dist().density(1));
		// console.log(cqlsHypo.m.exp2.distrib.step());
		// console.log(cqlsHypo.m.exp2.mode);
		// console.log(cqlsHypo.m.exp2.$pdf([0,1/10,2/10]));

		// console.log(cqlsHypo.m.exp3.$pdf([0,1,2]));
		// console.log(cqlsHypo.m.exp2.$mean());
		// console.log(cqlsHypo.m.exp3.$mean());
		// console.log(cqlsHypo.m.exp2.$bounds());
		// console.log(cqlsHypo.m.exp3.$bounds());
		// console.log(cqlsHypo.m.exp.$variance());
		// console.log(cqlsHypo.m.exp2.$quantile(0.95));
		// console.log(cqlsHypo.m.exp3.$quantile(0.95));

	   	cqlsHypo.m.canvas = document.getElementById("createjsCanvasHypo");

			//console.log("ow:"+typeof(ow));
			if(typeof(cqlsHypo.i.container) != "undefined") {
				cqlsHypo.m.ow = cqlsHypo.i.container.width();
	    	cqlsHypo.m.oh = cqlsHypo.m.ow/2;
			} else {
	    	cqlsHypo.m.ow = cqlsHypo.m.canvas.width;
				cqlsHypo.m.oh = cqlsHypo.m.canvas.height;
			}

		cqlsHypo.i.dim = {w: cqlsHypo.m.ow, h: cqlsHypo.m.oh/2};

		//Run function when browser resizes
		window.onresize=function() {
			if(typeof(cqlsAEP) != "undefined") cqlsAEP.f.resizeCanvas();
			cqlsHypo.f.resizeCanvas();
		};

	    cqlsHypo.m.stage = new createjs.Stage(cqlsHypo.m.canvas);
	    cqlsHypo.m.stage.enableMouseOver();
	    //cqlsHypo.m.stage.autoClear = true;
	    createjs.Touch.enable(cqlsHypo.m.stage);

	    cqlsHypo.s.plot=Opal.CqlsHypo.Plot.$new();
	    cqlsHypo.m.stage.addChild(cqlsHypo.s.plot.parent);



	    //Listener for sim plot
     //    cqlsHypo.s.plot.frame.addEventListener("click", function(evt) {
	    // 	if(cqlsHypo.i.anim) {
	    // 		if(evt.stageX > cqlsHypo.s.plot.dim["$[]"]("w")/2) cqlsHypo.i.indSim+=1; else cqlsHypo.i.indSim -=1;
		   //  	if(cqlsHypo.i.indSim<0) cqlsHypo.i.indSim=0;
		   //  	if(cqlsHypo.i.indSim>cqlsHypo.m.nbsSim[cqlsHypo.m.play.n.toString()].length-1) cqlsHypo.i.indSim=cqlsHypo.m.nbsSim[cqlsHypo.m.play.n.toString()].length-1;

	    // 	} else {
		   //  	if(evt.stageX > cqlsHypo.s.plot.dim["$[]"]("w")/2) cqlsHypo.i.count+=1; else cqlsHypo.i.count -=1;
		   //  	if(cqlsHypo.i.count<0) cqlsHypo.i.count=0;
		   //  	if(cqlsHypo.i.count>4) cqlsHypo.i.count=4;
	    // 	}
	    // });


	    ///cqlsHypo.h.plot.init();
	    cqlsHypo.h.plot=Opal.CqlsHypo.Plot.$new(Opal.hash2(["x","y","w","h"],{x:0,y:cqlsHypo.i.dim.h,w:cqlsHypo.i.dim.w,h:cqlsHypo.i.dim.h}),Opal.hash2(["bg"],{bg:"#8888FF"}));
	    cqlsHypo.m.stage.addChild(cqlsHypo.h.plot.parent);

	    // Listener for hist plot
	   	// cqlsHypo.h.plot.frame.addEventListener("click", function(evt) {
	   	// 	if(evt.stageX > cqlsHypo.s.plot.dim["$[]"]("w")/2) cqlsHypo.m.play.histCur.$level(1); else cqlsHypo.m.play.histCur.$level(-1);
	   	// 	if(cqlsHypo.i.allowLevelChange) {
		   //  	if(!cqlsHypo.i.anim) cqlsHypo.m.play.histCur.$draw();
		   //  	else {
		   //  		if(cqlsHypo.i.paused) cqlsHypo.m.play.$drawHist();
		   //  	}
		   //  }
		   //  cqlsHypo.m.stage.update();
	    // });

		cqlsHypo.m.tooltip = new createjs.Text("", "20px monospace", "#000")

	   	cqlsHypo.m.play=Opal.CqlsHypo.Play.$new();

		cqlsHypo.m.stage.addChild(cqlsHypo.m.tooltip);
		cqlsHypo.m.tooltip.scaleX=scaleTextX;cqlsHypo.m.tooltip.scaleY=scaleTextY;
		//Initial call
		if(typeof(cqlsHypo.i.container) != "undefined") console.log("hypo-container:"+cqlsHypo.i.container.width()+"x"+cqlsHypo.i.container.height());
		console.log("hypo (ow x oh):"+cqlsHypo.m.ow+"x"+cqlsHypo.m.oh);
		cqlsHypo.f.resizeCanvas();
		console.log("hypo:"+cqlsHypo.m.stage.canvas.width+"x"+cqlsHypo.m.stage.canvas.height);
		console.log("hypo (ow x oh) (AP):"+cqlsHypo.m.ow+"x"+cqlsHypo.m.oh);
		//if(typeof(cqlsHypo.i.container) != "undefined") console.log("hypo-container (AP):"+cqlsHypo.i.container.width()+"x"+cqlsHypo.i.container.height());
		if(cqlsHypo.mode=="enyo") {
			cqlsHypo.enyo.app.$.sdInput.hide();cqlsHypo.enyo.app.$.sdLeft.hide();
			//cqlsHypo.enyo.app.$.alphaMenu.hide();
			//cqlsHypo.enyo.app.$.pauseButton.hide();
		}
	}
