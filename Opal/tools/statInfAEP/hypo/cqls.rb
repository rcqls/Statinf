module CqlsHypo

	module Tooltip

		def initTooltip(shape=@shape)
			#shape needs to be an Array
			shape.each do |sh|
				%x{
					#{sh}.on("rollover",function(evt) {
						//console.log("mouseover!!!"+evt.stageX/cqlsHypo.m.stage.scaleX+":"+evt.stageY/cqlsHypo.m.stage.scaleY);
						cqlsHypo.m.tooltip.text=#{tooltipContent(sh,%x{evt})};
						cqlsHypo.m.tooltip.x=evt.stageX/cqlsHypo.m.stage.scaleX;
						cqlsHypo.m.tooltip.y=evt.stageY/cqlsHypo.m.stage.scaleY;
						cqlsHypo.m.tooltip.visible=true;
						//console.log("end mouseover");
						cqlsHypo.m.stage.update();
					});
					#{sh}.on("pressmove",function(evt) {
						//console.log("mouseover!!!"+evt.stageX/cqlsHypo.m.stage.scaleX+":"+evt.stageY/cqlsHypo.m.stage.scaleY);
						cqlsHypo.m.tooltip.text=#{tooltipContent(sh,%x{evt})};
						cqlsHypo.m.tooltip.x=evt.stageX/cqlsHypo.m.stage.scaleX;
						cqlsHypo.m.tooltip.y=evt.stageY/cqlsHypo.m.stage.scaleY;
						cqlsHypo.m.tooltip.visible=true;
						//console.log("end mouseover");
						cqlsHypo.m.stage.update();
					});
					#{sh}.on("rollout",function(evt) {
						//console.log("mouseout!!!");
						cqlsHypo.m.tooltip.text="";
						cqlsHypo.m.tooltip.visible=false;
						cqlsHypo.m.stage.update();
					});
				}
			end
		end

	end

	class Plot

		attr_accessor :parent, :frame, :style, :graph, :dim

		include Tooltip

		def initialize(dim={x:0,y:0,w:%x{cqlsHypo.i.dim.w},h:%x{cqlsHypo.i.dim.h}},style={bg:"#88FF88"})
			@dim,@style=dim,style
			@parent=%x{new createjs.Container()}
			@frame=	%x{new createjs.Shape()}
    		@graph=CqlsHypo::Graph.new(@dim)
    		@updateCalls=[]
    		# init frame
    		%x{#{@frame}.graphics.beginLinearGradientFill(["#FFF",#{@style[:bg]}], [0, 1], 0, #{@dim[:y]}+20, 0, #{@dim[:y]}+#{@dim[:h]}+20).drawRect(#{@dim[:x]},#{@dim[:y]},#{@dim[:w]},#{@dim[:h]})}
			addChild(@frame)
			@axisShape=%x{new createjs.Shape()}
			initTooltip([@axisShape])
		end

		def attachAxis
			addChild(@axisShape,[self,:drawAxis])
		end

		## updateCall is called when object needs to be updated before drawing
		## in particular when depending on graph with xylim fixed when all children provided
		## ex: [self,:drawAxis] or [self,:drawAxis,[]]
 		def addChild(child,updateCall=nil,pos=-1)
   			shape=child
   			@updateCalls << [child,updateCall] if updateCall
    		unless  %x{#{child}.shape == null}
  				shape=child.shape
    			child.plot=self
    			child.graph=@graph
    			@graph.add(child)
    		end
    		#%x{console.log("addChild"+shape)}
    		if pos>=0
    			%x{#{@parent}.addChildAt(#{shape},#{pos})}
    		else
    			#%x{console.log("add child "+shape)}
    			%x{#{@parent}.addChild(#{shape})}
    		end
    	end

   # Weirdly, does not work!!
   #  	def moveChildAt(child,ind)
   #  		shape=child
   #  		shape=child.shape unless  %x{#{child}.shape == null}
   #  		curInd=%x{#{@parent}.getChildIndex(#{shape})}
   #  		%x{console.log("curInd:"+curInd)}
   #  		inc,last=+1,%x{#{@parent}.getNumChildren()}-1
   #  		inc,last=-1,0 if ind < curInd
   #  		#%x{console.log("child at "+shape+":"+ind)}
   #  		while %x{#{@parent}.getChildIndex(#{shape})}!=ind
   #  			newInd=curInd+inc
			# 	%x{#{@parent}.swapChildrenAt(#{curInd},#{newInd})}
			# 	curInd=newInd
			# end
			# %x{console.log("curInd:"+#{@parent}.getChildIndex(#{shape}))}
   #  	end

    	# def removeChild(child)
    	# 	@updateCalls.delete(child)
    	# end

    	def update
    		##p ["update",self]
    		@updateCalls.each do |k,v|
    			#p [:update,v]
    			args=v[2]
    			args=[] unless args
    			v[0].method(v[1]).call(*args)
    		end
    		##p ["update2",self]
    	end

    	def drawAxis
    		%x{#{@axisShape}.graphics.ss(3,2).s("#000").mt(#{@dim[:x]},#{@graph.to_Y(0.0)}).lt(#{@dim[:x]+@dim[:w]},#{@graph.to_Y(0.0)}).es()}
    	end

    	def tooltipContent(shape,evt)
    		@graph.to_x(%x{#{evt}.stageX/cqlsHypo.m.stage.scaleX}).to_f
    	end

	end


	class Graph

		attr_accessor :xylim, :dim, :active, :syncedChildren, :zoom, :marg

		def Graph.adjust(inter,more=0)
			l=(inter[1]-inter[0])*more
			[inter[0]-more,inter[1]+more]
		end

		def initialize(dim,xlim=[],ylim=[],style=nil)
			@dim,@style=dim,style
			@marg={l:0.1,r:0.1,t:0.2,b:0.1}
			@marg[:l]=@dim[:w]*@marg[:l] if @marg[:l] < 1
			@marg[:r]=@dim[:w]*@marg[:r] if @marg[:r] < 1
			@marg[:t]=@dim[:h]*@marg[:t] if @marg[:t] < 1
			@marg[:b]=@dim[:h]*@marg[:b] if @marg[:b] < 1
			@xylim0={x: xlim, y: ylim}
			@list,@active=[],[]
			@list << @xylim0 unless @xylim0[:x].empty?
			@xylim={x: [], y: []}
			@tr={}
			@zoom={x0: 0.0, x1: 0.0, y0: 0.0, y1: 0.0, active: false}
			@syncedChildren=[]
		end

		def syncTo(graph)
			@xylim=graph.xylim
			@zoom=graph.zoom
			graph.syncedChildren << self
			@synced=true
		end

		def synced?
			@synced
		end

		def update(active=@active)
			##p @xylim
			unless synced?
			#%x{console.log(#{@list})};p @dim;
			##p [:active,active]
			##p [:List,@list]
				list=@list.select{|e| active.empty? or (active.include? e[1])}
				#%x{console.log(#{list})}
				##p [:list,list]
				@xylim[:x][0]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:x][0]}.min
				@xylim[:x][1]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:x][1]}.max
				@xylim[:y][0]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:y][0]}.min
				@xylim[:y][1]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:y][1]}.max
			end
			# then update the rescaling coeff
			##p @xylim
			@tr[:ax],@tr[:ay]=(@xylim[:x][1]+@zoom[:x1]-@xylim[:x][0]-@zoom[:x0])/(@dim[:w]-@marg[:l]-@marg[:r]),(@xylim[:y][0]+@zoom[:y0]-@xylim[:y][1]-@zoom[:y1])/(@dim[:h]-@marg[:t]-@marg[:b])
			@tr[:bx],@tr[:by]=@xylim[:x][0]+@zoom[:x0]-@tr[:ax]*(@dim[:x]+@marg[:l]),@xylim[:y][1]+@zoom[:y1]-@tr[:ay]*(@dim[:y]+@marg[:t])
			#p @tr
			@syncedChildren.each {|c| c.update} unless @syncedChildren.empty?
		end

		def setActive(ary)
			@active=ary
		end

		def add(element,mode=:element,id=nil)
			return if synced?
			case mode
			when :element
				if element.xylim
					@list << [:element,id||element.id,element]
					update
				end
			when :xylim
				@list << [:xylim,id,element]
				update
			end
		end

		def addXYLim(id,x0,x1,y0,y1)
			add({x:[x0,x1],y:[y0,y1]},:xylim,id)
		end

		def to_x(x)
			@tr[:ax]*x+@tr[:bx]
		end

		def to_X(x)
			(x-@tr[:bx])/@tr[:ax]
		end

		def to_y(y)
			@tr[:ay]*y+@tr[:by]
		end

		def to_Y(y)
			(y-@tr[:by])/@tr[:ay]
		end

		# (x,y) global to local (i.e. graph)
		def to_local(x,y)
			[@tr[:ax]*x+@tr[:bx],@tr[:ay]*y+@tr[:by]]
		end

		# (x,y) from local (i.e. graph) to global
		def to_global(x,y)
			[(x-@tr[:bx])/@tr[:ax],(y-@tr[:by])/@tr[:ay]]
		end

		def zoomActive
			@zoom[:active]
		end

		def toggleZoomTo(plot,type=[:xpos,:xneg,:ypos,:reset])#,:yneg])
			@zoom[:active]=!@zoom[:active]
			if @zoom[:active]
				unless @zoomShapes
					@zoomShapes={}
					keys=[]
					keys += [:xposmore, :xposless] if type.include? :xpos
					keys += [:xnegmore, :xnegless] if type.include? :xneg
					keys += [:yposmore, :yposless] if type.include? :ypos
					keys += [:ynegmore, :ynegless] if type.include? :yneg
					keys += [:reset] if type.include? :reset
					keys.each {|k| @zoomShapes[k]=%x{new createjs.Shape()}}
				end
				@zoomShapes.each_key do |k|
					%x{
						plot.parent.addChild(#{@zoomShapes[k]})
					}
				end
				showZoom
			else
				@zoomShapes.each_key do |k|
					%x{
						plot.parent.removeChild(#{@zoomShapes[k]})
					}
				end

			end
		end

		def showZoom
			size=40
			inter=15
			@zoomShapes.each_key do |k|
				%x{#{@zoomShapes[k]}.alpha=0.5}

				case k
				when :xposmore
					# %x{#{@zoomShapes[:xposmore]}.graphics.s("#000").f("#FFF").drawRect(#{@dim[:w]}-1.5*#{size},#{@dim[:h]/2.0}-#{size/2},#{size},#{size})}
					 %x{#{@zoomShapes[:xposmore]}.graphics.c().s("#000").f("#FFF").mt(#{@dim[:w]}-1.5*#{size},#{@dim[:h]/2.0}-#{size/2}).lt(#{@dim[:w]}-1.5*#{size},#{@dim[:h]/2.0}+#{size/2}).lt(#{@dim[:w]}-0.5*#{size},#{@dim[:h]/2.0}).cp() }
				when :xposless
					# %x{#{@zoomShapes[:xposless]}.graphics.s("#000").f("#FFF").drawRect(#{@dim[:w]}-2.5*#{size}-#{inter},#{@dim[:h]/2.0}-#{size/2},#{size},#{size})}
					%x{#{@zoomShapes[:xposless]}.graphics.c().s("#000").f("#FFF").mt(#{@dim[:w]}-1.5*#{size}-#{inter},#{@dim[:h]/2.0}-#{size/2}).lt(#{@dim[:w]}-1.5*#{size}-#{inter},#{@dim[:h]/2.0}+#{size/2}).lt(#{@dim[:w]}-2.5*#{size}-#{inter},#{@dim[:h]/2.0}).cp() }
				when :xnegmore
					# %x{#{@zoomShapes[:xnegmore]}.graphics.s("#000").f("#FFF").drawRect(0.5*#{size},#{@dim[:h]/2.0}-#{size/2},#{size},#{size})}
					%x{#{@zoomShapes[:xnegmore]}.graphics.c().s("#000").f("#FFF").mt(1.5*#{size},#{@dim[:h]/2.0}-#{size/2}).lt(1.5*#{size},#{@dim[:h]/2.0}+#{size/2}).lt(0.5*#{size},#{@dim[:h]/2.0}).cp() }
				when :xnegless
					# %x{#{@zoomShapes[:xnegless]}.graphics.s("#000").f("#FFF").drawRect(1.5*#{size}+#{inter},#{@dim[:h]/2.0}-#{size/2},#{size},#{size})}
					%x{#{@zoomShapes[:xnegless]}.graphics.c().s("#000").f("#FFF").mt(1.5*#{size}+#{inter},#{@dim[:h]/2.0}-#{size/2}).lt(1.5*#{size}+#{inter},#{@dim[:h]/2.0}+#{size/2}).lt(2.5*#{size}+#{inter},#{@dim[:h]/2.0}).cp() }
				when :ynegmore
					# %x{#{@zoomShapes[:ynegmore]}.graphics.s("#000").f("#FFF").drawRect(#{@dim[:w]/2.0}-#{size/2},#{@dim[:h]}-1.5*#{size},#{size},#{size})}
					%x{#{@zoomShapes[:ynegmore]}.graphics.c().s("#000").f("#FFF").mt(#{@dim[:w]/2.0}-#{size/2},#{@dim[:h]}-1.5*#{size}).lt(#{@dim[:w]/2.0}+#{size/2},#{@dim[:h]}-1.5*#{size}).lt(#{@dim[:w]/2.0},#{@dim[:h]}-0.5*#{size}).cp() }
				when :ynegless
					# %x{#{@zoomShapes[:ynegless]}.graphics.s("#000").f("#FFF").drawRect(#{@dim[:w]/2.0}-#{size/2},#{@dim[:h]}-2.5*#{size}-#{inter},#{size},#{size})}
					%x{#{@zoomShapes[:ynegless]}.graphics.c().s("#000").f("#FFF").mt(#{@dim[:w]/2.0}-#{size/2},#{@dim[:h]}-1.5*#{size}-#{inter}).lt(#{@dim[:w]/2.0}+#{size/2},#{@dim[:h]}-1.5*#{size}-#{inter}).lt(#{@dim[:w]/2.0},#{@dim[:h]}-2.5*#{size}-#{inter}).cp() }
				when :yposmore
					# %x{#{@zoomShapes[:yposmore]}.graphics.s("#000").f("#FFF").drawRect(#{@dim[:w]/2.0}-#{size/2},#{0.5*size},#{size},#{size})}
					%x{#{@zoomShapes[:yposmore]}.graphics.c().s("#000").f("#FFF").mt(#{@dim[:w]/2.0}-#{size/2},1.5*#{size}).lt(#{@dim[:w]/2.0}+#{size/2},1.5*#{size}).lt(#{@dim[:w]/2.0},0.5*#{size}).cp() }
				when :yposless
					# %x{#{@zoomShapes[:yposless]}.graphics.s("#000").f("#FFF").drawRect(#{@dim[:w]/2.0}-#{size/2},#{1.5*size}+#{inter},#{size},#{size})}
					%x{#{@zoomShapes[:yposless]}.graphics.c().s("#000").f("#FFF").mt(#{@dim[:w]/2.0}-#{size/2},1.5*#{size}+#{inter}).lt(#{@dim[:w]/2.0}+#{size/2},1.5*#{size}+#{inter}).lt(#{@dim[:w]/2.0},2.5*#{size}+#{inter}).cp() }
				when :reset
					%x{#{@zoomShapes[:reset]}.graphics.c().s("#000").f("#FFF").drawRect(#{@dim[:w]/2.0}-#{size/2}, #{@dim[:h]/2.0}-#{size/2},#{size},#{size}) }
				end
			end
		end

		def hitZoom(x,y)
			return unless @zoom[:active]
			select=:none
			@zoomShapes.each_key do |k|
				%x{if(#{@zoomShapes[k]}.hitTest(#{x}, #{y})) {#{select}=#{k}};}
				 break unless select==:none
			end
			return select if select==:none

			step=0.1/2

			case select
			when :xposmore
				@zoom[:x1]+=step*(@xylim[:x][1]-@xylim[:x][0])
			when :xposless
				@zoom[:x1]=@zoom[:x1]-step*(@xylim[:x][1]-@xylim[:x][0]) unless @zoom[:x1] < (step-1/2)*(@xylim[:x][1]-@xylim[:x][0])
			when :xnegmore
				@zoom[:x0]=@zoom[:x0]-step*(@xylim[:x][1]-@xylim[:x][0])
			when :xnegless
				@zoom[:x0]+=step*(@xylim[:x][1]-@xylim[:x][0]) unless @zoom[:x0] > (1/2-step)*(@xylim[:x][1]-@xylim[:x][0])
			when :yposmore
				@zoom[:y1]+=step*(@xylim[:y][1]-@xylim[:y][0])
			when :yposless
				@zoom[:y1]=@zoom[:y1]-step*(@xylim[:y][1]-@xylim[:y][0]) unless @zoom[:y1] < (step-1/2)*(@xylim[:y][1]-@xylim[:y][0])
			when :ynegmore
				@zoom[:y0]=@zoom[:y1]-step*(@xylim[:y][1]-@xylim[:y][0])
			when :ynegless
				@zoom[:y0]+=step*(@xylim[:y][1]-@xylim[:y][0]) unless @zoom[:y0] > (1/2-step)*(@xylim[:y][1]-@xylim[:y][0])
			when :reset
				@zoom[:x0]=@zoom[:x1]=@zoom[:y0]=@zoom[:y1]=0.0
			end

			return select
		end

	end


	class Child

		attr_accessor :id, :plot, :graph, :shape, :style, :xylim

		include Tooltip

		def initialize

		end

		def setPlot(plot)
			@plot=plot
			@graph=@plot.graph
		end

	end

	## This behaves as an Experiment
	## since it responds to xy method
	class Curve < Child

		attr_accessor :distrib, :bounds, :kind, :type, :style, :meanStyle, :sdStyle, :summaryShapes

		def initialize(id=nil,type=:cont,bounds=[0,1],length=512)
			@@curve_cpt=-1 unless @@curve_cpt
			@id=id || "curve"+(@@curve_cpt+=1).to_s
			@type=type
			case @type
			when :cont
				@bounds,@length=bounds,length
			when :disc
				@bounds=bounds #sequence of ordered values
				@length=@bounds.length #fortunately useless
				initStep
			end

			@style={close: true,stroke:"#000",fill:"rgba(200,200,255,0.3)",thickness: 3}
			@meanStyle={thickness: 3,stroke: "#000"}
			@sdStyle={thickness: 3,stroke: "#000"}

			@shape=%x{new createjs.Shape()}
			@x=CqlsHypo.seq(@bounds[0],@bounds[1],@length)
			@kind=:density
			@summaryShapes=[%x{new createjs.Shape()},%x{new createjs.Shape()}]
			@axisShape=%x{new createjs.Shape()}
			initTooltip(@summaryShapes)

		end

		def attachAxis(ratio)
			@plot.addChild(@axisShape,[self,:drawAxis,[ratio]])
		end

		def drawAxis(ratio)
			%x{#{@axisShape}.visible=true}
			%x{#{@axisShape}.graphics.c().s("#000").ss(1).mt(#{@graph.dim[:x]},#{@graph.dim[:h]*ratio}).lt(#{@graph.dim[:x]+@graph.dim[:w]},#{@graph.dim[:h]*ratio})}
		end

		def attachShapes
			@plot.addChild(@summaryShapes[0],[self,:drawMean])
			@plot.addChild(@summaryShapes[1],[self,:drawSD])
		end

		def drawMean
			# %x{#{@summaryShapes[0]}.visible=true}
			#p [:drawMean,0,@graph.dim[:y],@graph.dim[:y]+@graph.dim[:h],@graph.to_X(@distrib.mean),@distrib.mean]
			%x{
				#{@summaryShapes[0]}.graphics.c().s(#{@meanStyle[:stroke]}).ss(#{@meanStyle[:thickness]}).mt(0,#{@graph.dim[:y]}).lt(0,#{@graph.dim[:y]+@graph.dim[:h]})
				#{@summaryShapes[0]}.x=#{@graph.to_X(@distrib.mean)};
				//#{@summaryShapes[0]}.y=#{@graph.dim[:y]};
			}
		end

		def drawSD
			x,y=10,10
			h=@distrib.maxPdf/2.0
			h=h/@step if @type==:disc
			h=@graph.to_Y(h)
			# %x{#{@summaryShapes[1]}.visible=true}
			%x{
				#{@summaryShapes[1]}.graphics.c().s(#{@sdStyle[:stroke]}).ss(#{@sdStyle[:thickness]})
				.mt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)-@graph.to_X(@distrib.mean)}+x,#{h}-y)
				.lt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)-@graph.to_X(@distrib.mean)},#{h})
				.lt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)-@graph.to_X(@distrib.mean)}+x,#{h}+y)
				.mt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)-@graph.to_X(@distrib.mean)},#{h})
				.lt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)-@graph.to_X(@distrib.mean)},#{h})
				.lt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)-@graph.to_X(@distrib.mean)}-x,#{h}-y)
				.mt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)-@graph.to_X(@distrib.mean)},#{h})
				.lt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)-@graph.to_X(@distrib.mean)}-x,#{h}+y)
				#{@summaryShapes[1]}.x=#{@graph.to_X(@distrib.mean)}
			}
		end

		#######################################
		## This is required for an experiment
		def sample(n=1)
			@distrib.sample(n)
		end

		def y(x)
			y=@distrib.pdf(x)
			y.map!{|e| e/@distrib.step} if @distrib.type==:disc
			y=%x{#{y}.map(function(e) {return Math.random()*e;})}
			y
		end

		def xy(n=1)
			x=sample(n)
			y=y(x)
			{x: x, y: y}
		end
		#######################################

		def initStep
			@step=(1...@bounds.length).map {|i| (@bounds[i]-@bounds[i-1]).abs}.min.to_f
		end

		def setDistrib(name,params)
			@distrib=Distribution.new
			@distrib.set(name,params)
			initDistrib
		end

		def setDistribAs(dist)
			@distrib=dist
			initDistrib
		end

		def setDistribAsTransf(transf,dist)
			@distrib=Distribution.new
			@distrib.setAsTransfOf(dist,transf)
			initDistrib
		end

		def regular?
			@distrib.regular?
		end

		def initDistrib
			@type=@distrib.type
			@bounds=@distrib.bounds
			# p [:type,@type]
			# p [:bounds,@bounds]
			case @type
			when :cont
				@x=CqlsHypo.seq(@bounds[0],@bounds[1],@length)
			when :disc
				initStep
				# p @step
				@x=@bounds
			end
			@y=@distrib.pdf(@x)
			@y.map!{|e| e/@step} if @type==:disc
			initXYLim
			#  p @x
			#  p @y
			#  p (@y.inject(0) {|e,e2| e+=e2})
			# p @xylim
		end

		def initXYLim
			xlim=@type==:cont ? @bounds : [@bounds[0]-@step/2.0,@bounds[-1]+@step/2.0]
			@xylim={x: Graph.adjust(xlim), y: Graph.adjust([0,@y.max])}
		end

		def draw(shape=@shape,graph=@graph,style=@style)
			if @type==:cont
				drawCont(shape,graph,style)
			else
				drawDisc(shape,graph,style)
			end
		end

		def drawCont(shape=@shape,graph=@graph,style=@style)
			%x{
				#{shape}.graphics.clear();
				if(#{style[:close]}) {#{shape}.graphics.f(#{style[:fill]});}
				#{shape}.graphics.s(#{style[:stroke]}).ss(#{style[:thickness]});
			}
			%x{#{shape}.x=#{graph.to_X(@distrib.mean)}}
			%x{#{shape}.graphics.mt(#{graph.to_X(@x[0])}-#{shape}.x,#{graph.to_Y(0.0)})}
			# Carefull, @x.length may differ from @length => do not use @length below
			(0...@x.length).each {|i|
				#p [i,"a",@x[i],@y[i]]
				#p ["b",graph.to_X(@x[i]),graph.to_Y(@y[i])]
				%x{#{shape}.graphics.lt(#{graph.to_X(@x[i])}-#{shape}.x,#{graph.to_Y(@y[i])})}
			}
			%x{#{shape}.graphics.lt(#{graph.to_X(@x[-1])}-#{shape}.x,#{graph.to_Y(0.0)})}
			%x{#{shape}.graphics.cp()} if style[:close]
		end

		def drawDisc(shape=@shape,graph=@graph,style=@style)
			s=@step/2.0
			%x{
				#{shape}.graphics.clear();
				if(#{style[:close]}) {#{shape}.graphics.f(#{style[:fill]});}
				#{shape}.graphics.s(#{style[:stroke]}).ss(#{style[:thickness]});
			}
			(0...@x.length).each {|i|
				%x{
				 	#{shape}.graphics.mt(#{graph.to_X(@x[i]-s)},#{graph.to_Y(0.0)})
					.lt(#{graph.to_X(@x[i]-s)},#{graph.to_Y(@y[i])})
					.lt(#{graph.to_X(@x[i]+s)},#{graph.to_Y(@y[i])})
			 		.lt(#{graph.to_X(@x[i]+s)},#{graph.to_Y(0.0)})
			 	}
				%x{#{shape}.graphics.cp()} if style[:close]
			}
		end

		## TODO: more complete later! Now just for AreaRisk
		## Cont from now! Rmk: it is not so difficult to extend
		def drawAreaSide(lim,side,shape,style=@style,graph=@graph) #side=:left or :right
			#p [:drawArea,lim,side]
			case side
			when :left
				from,to=0,0
				to+=1 while to<@x.length-1 and @x[to]<=lim
			when :right
				from,to=@x.length-1,@x.length-1
				from-=1 while from>0 and @x[from]>=lim
			when :between
				from,to=0,@x.length-1
				from+=1 while from<@x.length-1 and @x[from]<lim[0]
				to-=1 while to>0 and @x[to]>lim[1]
			end
			%x{
				#{shape}.graphics.clear();
				#{shape}.graphics.f(#{style[:fill]});
				#{shape}.graphics.s(#{style[:stroke]}).ss(#{style[:thickness]});
			}
			%x{#{shape}.x=#{graph.to_X(@distrib.mean)}}
			%x{#{shape}.graphics.mt(#{graph.to_X(@x[from])}-#{shape}.x,#{graph.to_Y(0.0)})}
			# Carefull, @x.length may differ from @length => do not use @length below
			(from..to).each {|i|
				#p [i,"a",@x[i],@y[i]]
				#p ["b",graph.to_X(@x[i]),graph.to_Y(@y[i])]
				%x{#{shape}.graphics.lt(#{graph.to_X(@x[i])}-#{shape}.x,#{graph.to_Y(@y[i])})}
			}
			%x{#{shape}.graphics.lt(#{graph.to_X(@x[to])}-#{shape}.x,#{graph.to_Y(0.0)})}
			%x{#{shape}.graphics.cp()}

		end

		def tooltipContent(shape,evt)
			if %x{#{shape} == #{@summaryShapes[0]}}
				@distrib.mean.to_s
			elsif %x{#{shape}==#{@summaryShapes[1]}}
				@distrib.stdDev.to_s
			end
		end

	end

	module Callables

		def suspendCallables(tag=:default)
			@activeCallables[tag]=false
		end

		def resumeCallables(tag=:default)
			@activeCallables[tag]=true
		end

		def addCallable(call,tag=:default)
			@callables,@activeCallables={},{} unless @callables
			@callables[tag],@activeCallables[tag]=[],[] unless @callables[tag]
			@callables[tag] << call
		end

		def playCallables(tag=:default)
			if @callables[tag] and @activeCallables[tag]
				@callables[tag].each do |v|
	    			args=v[2]
	    			args=[] unless args
	    			v[0].method(v[1]).call(*args)
	    		end
	    	end
		end

	end

	class StatTestCurve < Curve

		attr_accessor :typeStatTest, :paramsFrame, :paramsStatTest

		include Callables

		def initialize(type=:p,params=[1000,0.15])
			super()
			setParamsFrame(type,params)
			@styles
		end

		def setStyle
		end

		def initEvents(types=[:mean,:sd])
			types.each do |type|
				case type
				when :mean
					%x{
						#{@summaryShapes[0]}.on("pressmove", function(evt) {
							var x=evt.stageX/cqlsHypo.m.stage.scaleX;
							if(#{@typeStatTest==:p}){
								#{@paramsFrame[1]=@graph.to_x(%x{x})}
								#{updateStatTestDistrib}
								//console.log("mean="+#{@distrib.mean}+",sd="+#{@distrib.stdDev})
								#{draw};
								#{drawMean()};
								#{drawSD()};
								#{playCallables};
								cqlsHypo.m.stage.update();
						    } else if(#{@typeStatTest==:m}) {
						    	//console.log("MEAN pressed");
						    	#{@paramsFrame[1]=@graph.to_x(%x{x})}
								#{updateStatTestDistrib}
								//console.log("mean="+#{@distrib.mean}+",sd="+#{@distrib.stdDev})
								#{draw};
								#{drawMean()};
								#{drawSD()};
								//console.log("MEAN pressed -> delta:"+#{@delta});
								#{playCallables};
								//console.log("MEAN OUT");
							}
						    cqlsHypo.m.stage.update();
						});
						#{@summaryShapes[0]}.on("pressup", function(evt) {
							var x=evt.stageX/cqlsHypo.m.stage.scaleX;
							//console.log("TTTTTypeStatTest:"+#{@typeStatTest})
							if(#{@typeStatTest==:p}){
								//console.log("prop up");
								#{@paramsFrame[1]=@graph.to_x(%x{x})}
								#{updateStatTestDistrib}
								//console.log("mean="+#{@distrib.mean}+",sd="+#{@distrib.stdDev})
								#{draw};
								#{drawSD};
							} else if(#{@typeStatTest==:m}) {
								//console.log("MEANNNN UUUUUPPPPP");
								#{@paramsFrame[1]=@graph.to_x(%x{x})}
								#{updateStatTestDistrib}
								//console.log("mean="+#{@distrib.mean}+",sd="+#{@distrib.stdDev})
							}
							cqlsHypo.m.stage.update();
						});
					}
				when :sd
					%x{
						#{@summaryShapes[1]}.on("mousedown", function(evt) {
							if(#{@typeStatTest==:m}) {
								//console.log("sd down");
								#{@sdX}=evt.stageX/cqlsHypo.m.stage.scaleX;
								#{@oldSD=@graph.to_X(@distrib.stdDev)-@graph.to_X(0.0)};
							}
						});
						#{@summaryShapes[1]}.on("pressmove", function(evt) {
							var x=evt.stageX/cqlsHypo.m.stage.scaleX;
							if(#{@typeStatTest==:m}) {
								//console.log("sd pressed");

								var newSD=#{@oldSD}+x-#{@sdX};
						    	//#{@summaryShapes[1]}.scaleX=newSD/oldSD;
						    	//point at the right in the real scale then substracted from real mean
						    	//Do not forget the sqrt(n) because it is the
						    	#{@paramsFrame[2]=@graph.to_x(@graph.to_X(0.0)+@oldSD +%x{x}-@sdX)*%x{Math.sqrt(#{@paramsFrame[0]})}};
						    	#{updateStatTestDistrib}
						    	#{draw};
						    	#{drawSD};
						    	#{playCallables(:sd)};
						    	//console.log("mean="+#{@distrib.mean}+",sd="+#{@distrib.stdDev})
						    	cqlsHypo.m.stage.update();
						    }
						});
					}
				end
			end
		end

		def paramsFrameAtFrom(key,statTest2,key2=nil)
			key2=key if key2.nil?
			@paramsFrame[key]=statTest2.paramsFrame[key2]
		end

		def setParamsFrame(type=:p,params=[1000,0.15])
			#:p (prop) : [n,p]
			#:m (mean) : [n,mu,sd]
			#:v (variance) : TODO
			#:dp (delta-prop) : [n,p0,p]
			#:dp1 (delta-prop) : [[n,p],p0]
			#:dm (delta-mean) : [n,mu0,mu,sd]
			#:dm1 (delta-mean) : [[n,mu],mu0]
			@typeStatTest=type
			@paramsFrame=params
			updateStatTestDistrib
		end

		def updateStatTestDistrib
			#p [:typeStatTest,@typeStatTest,@paramsFrame]
			@paramsStatTest=case @typeStatTest
			when :p
				[@paramsFrame[1],%x{Math.sqrt(#{@paramsFrame[1]*(1-@paramsFrame[1])/@paramsFrame[0]})}]
			when :m
				#p [:mToUpdate,@paramsFrame[2]]
				[@paramsFrame[1],@paramsFrame[2]/%x{Math.sqrt(#{@paramsFrame[0]})}]
			when :dp0,:dm0
				[0,1]
			when :dp,:dp1
				paramsFrame=@typeStatTest==:dp1 ? [@paramsFrame[0].paramsFrame[0],@paramsFrame[1],@paramsFrame[0].paramsFrame[1]] : @paramsFrame
				[(paramsFrame[2]-paramsFrame[1])/%x{Math.sqrt(#{paramsFrame[1]*(1-paramsFrame[1])/paramsFrame[0]})},%x{Math.sqrt(#{(paramsFrame[2]*(1-paramsFrame[2]))/(paramsFrame[1]*(1-paramsFrame[1]))})}]
			when :dm,:dm1
				paramsFrame=@typeStatTest==:dm1 ? [@paramsFrame[0].paramsFrame[0],@paramsFrame[1],@paramsFrame[0].paramsFrame[1],@paramsFrame[0].paramsFrame[2]] : @paramsFrame
				[(paramsFrame[2]-paramsFrame[1])/paramsFrame[3]*%x{Math.sqrt(#{paramsFrame[0]})},1]
			end
			#p [:updateStat,@paramsStatTest]
			setDistrib("normal",@paramsStatTest)
		end

	end

	class AcceptanceRegion < Child

		attr_accessor :alpha, :side, :style, :shapes

		include Callables

		def initialize(statTestH0,context,id=nil)
			@@limitCpt=-1 unless @@limitCpt
			@id=id || "Lim"+(@@limitCpt+=1).to_s
			@statTestH0,@context=statTestH0,context
			@style={stroke:"#0F0",thickness: 3}
			@sides=[]
			@shapes=[%x{new createjs.Shape()},%x{new createjs.Shape()}]

			#default is from context
			@alpha=:context #or :paramPval or:deltaPval
			@side=:context
			initTooltip(@shapes)
		end

		def initEvents
			%x{

				#{@shapes[0]}.on("pressmove", function(evt) {
					var x=evt.stageX/cqlsHypo.m.stage.scaleX;

					#{setAlphaFromQuantile(@graph.to_x(%x{x}),:left)}
					#{draw}
					#{playCallables};
					cqlsHypo.m.stage.update();
			     });

				#{@shapes[1]}.on("pressmove", function(evt) {
					var x=evt.stageX/cqlsHypo.m.stage.scaleX;

					#{setAlphaFromQuantile(@graph.to_x(%x{x}),:right)}
					#{draw}
					#{playCallables};
					cqlsHypo.m.stage.update();
			     });
			}
		end

		def setAlpha(alpha)
			case @alpha
			when :context
				@context[:alpha]=alpha
			when :paramPval
				@context[:paramPval]=alpha
			when :deltaPval
				@context[:deltaPval]=alpha
			else
				@alpha=alpha
			end
		end

		def setAlphaFromQuantile(q,from=:right)
			case from
			when :right
				alpha=1-@statTestH0.distrib.cdf(q)
			when :left
				alpha=@statTestH0.distrib.cdf(q)
			end
			alpha=2*alpha if (@side == :context ? @context[:side] : @side)=="!="
			alpha=[alpha,1.0].min
			setAlpha(alpha)
		end

		def getSides
			side= @side == :context ? @context[:side] : @side
			alpha=case @alpha
			when :context
				@context[:alpha]
			when :paramPval
				@context[:paramPval]
			when :deltaPval
				@context[:deltaPval]
			else
				@alpha
			end

			case side
			when ">"
				@sides=[0,1-alpha]
			when "<"
				@sides=[alpha,0]
			when "!="
				@sides=[alpha/2.0,1-alpha/2.0]
			end
			#p [:getSides,@context[:side],alpha,@sides]
		end

		def draw
			getSides
			@sides.each_with_index do |s,i|
				if s>0
					#p [:side,i,s,@statTestH0.distrib.quantile(s),@graph.dim[:y],@graph.dim[:h]]
					%x{
						#{@shapes[i]}.graphics.c().s(#{@style[:stroke]}).ss(#{@style[:thickness]}).mt(0,#{@graph.dim[:y]}).lt(0,#{@graph.dim[:y]+@graph.dim[:h]})
						#{@shapes[i]}.x=#{@graph.to_X(@statTestH0.distrib.quantile(s))};
					}
				else
					%x{#{@shapes[i]}.graphics.c()}
				end
			end
		end

		def attachShapes
			@plot.addChild(@shapes[0],[self,:draw])
			@plot.addChild(@shapes[1],[self,:draw])
		end

		def tooltipContent(shape,evt)
			@graph.to_x(%x{#{shape}.x}).to_s
		end

	end

	class AreaRisk < Child

		attr_accessor :alpha, :side, :style, :shapes

		def initialize(statTest,context,id=nil)
			@@areaCpt=-1 unless @@areaCpt
			@id=id || "Risk"+(@@areaCpt+=1).to_s
			@statTest,@context=statTest,context
			@style={stroke:"rgba(255,0,0,.6)",fill:"rgba(255,0,0,.3)",thickness: 1}
			@sides=[]
			@shapes=[%x{new createjs.Shape()},%x{new createjs.Shape()}]

			#default is from context
			@alpha=:context #or paramH0 or deltaH0 or percentage
			@side=:context
			initTooltip(@shapes)
		end

		def getSides
			#p [:getSIDESSSS,@id]
			side= @side == :context ? @context[:side] : @side
			statTestQuantile=@statTest #statTest for the quantile, default the same!
			alpha=case @alpha
			when :context
				@context[:alpha]
			when :paramH0
				statTestQuantile=@context[:paramEstH0]
				@context[:alpha]
			when :deltaH0
				statTestQuantile=@context[:deltaEstH0]
				@context[:alpha]
			when :paramEstLim
				statTestQuantile=@context[:paramEstH0]
				@context[:paramPval]
			when :deltaEstLim
				statTestQuantile=@context[:deltaEstH0]
				@context[:deltaPval]
			else
				@alpha
			end

			# value of parameter of interest
			param=case @statTest.typeStatTest
			when :dp0,:dm0,:dp1,:dm1
				@statTest.paramsFrame[0].paramsFrame[1]
			else
				@statTest.paramsFrame[1]
			end

			#p [:getSides,@statTest.paramsFrame,@context[:ref],alpha]
			#p [:paramSSSSSSS,side,param,@context[:ref]]
			case side
			when ">"
				if param > @context[:ref]
					@sides=[statTestQuantile.distrib.quantile(1-alpha),nil]
				else
					@sides=[nil,statTestQuantile.distrib.quantile(1-alpha)]
				end
			when "<"
				if param < @context[:ref]
					@sides=[nil,statTestQuantile.distrib.quantile(alpha)]
				else
					@sides=[statTestQuantile.distrib.quantile(alpha),nil]
				end
			when "!="
				if %x{Math.abs(#{param - @context[:ref]})} < 0.0001
					@sides=[statTestQuantile.distrib.quantile(alpha/2.0),statTestQuantile.distrib.quantile(1-alpha/2.0)]
				else
					@sides=[:between,statTestQuantile.distrib.quantile(alpha/2.0),statTestQuantile.distrib.quantile(1-alpha/2.0)]
				end
			end
			#p [:getSidesSSSSSSS,@sides]
		end

		def draw
			getSides
			if @sides.length==3 #between area
				@statTest.drawAreaSide(@sides[1..-1],:between,@shapes[0],style=@style,graph=@graph)
				%x{#{@shapes[1]}.graphics.c()}
			else
				@sides.each_with_index do |side,i|
					if side
						@statTest.drawAreaSide(side,(i==0 ? :left : :right),@shapes[i],style=@style,graph=@graph)
					else
						%x{#{@shapes[i]}.graphics.c()}
					end
				end
			end
		end

		def attachShapes
			@plot.addChild(@shapes[0],[self,:draw])
			@plot.addChild(@shapes[1],[self,:draw])
		end

		def tooltipContent(shape,evt)
			alpha=case @alpha
			when :context
				@context[:alpha]
			when :paramH0
				statTestQuantile=@context[:paramEstH0]
				@context[:alpha]
			when :deltaH0
				statTestQuantile=@context[:deltaEstH0]
				@context[:alpha]
			when :paramEstLim
				statTestQuantile=@context[:paramEstH0]
				@context[:paramPval]
			when :deltaEstLim
				statTestQuantile=@context[:deltaEstH0]
				@context[:deltaPval]
			else
				@alpha
			end

			area=alpha #default
			if statTestQuantile and @statTest != statTestQuantile
				# value of parameter of interest
				param=case @statTest.typeStatTest
				when :dp0,:dm0,:dp1,:dm1
					@statTest.paramsFrame[0].paramsFrame[1]
				else
					@statTest.paramsFrame[1]
				end

				side= @side == :context ? @context[:side] : @side
				area=case side
				when ">"
					if param > @context[:ref]
						@statTest.distrib.cdf(statTestQuantile.distrib.quantile(1-alpha))
					else
						1-@statTest.distrib.cdf(statTestQuantile.distrib.quantile(1-alpha))
					end
				when "<"
					if param < @context[:ref]
						1-@statTest.distrib.cdf(statTestQuantile.distrib.quantile(alpha))
					else
						@statTest.distrib.cdf(statTestQuantile.distrib.quantile(alpha))
					end
				when "!="
					if %x{Math.abs(#{param - @context[:ref]})} < 0.0001
						2*@statTest.distrib.cdf(statTestQuantile.distrib.quantile(alpha/2.0))
					else
						@statTest.distrib.cdf(statTestQuantile.distrib.quantile(1-alpha/2.0))-@statTest.distrib.cdf(statTestQuantile.distrib.quantile(alpha/2.0))
					end
				end
			end
			(area*100).to_s+"%"
		end
	end

	class Play

		attr_accessor :exp

		def initialize(plotParam=%x{cqlsHypo.s.plot},plotDelta=%x{cqlsHypo.h.plot})

			@stage=%x{cqlsHypo.m.stage}
			@plotParam,@plotDelta=plotParam,plotDelta
			@graphParam,@graphDelta=@plotParam.graph,@plotDelta.graph

			setStyles

			## exp needs to be set, @kind describes the kind of experiment
			@paramEst=[StatTestCurve.new(:p,[1000,0.15],false),StatTestCurve.new(:p,[1000,0.2])]
			@plotParam.addChild(@paramEst[0]);@plotParam.addChild(@paramEst[1])
			@paramEst[1].style[:fill]=%x{createjs.Graphics.getRGB(200,200,200,.3)};
			@paramEst[1].style[:thickness]=1
			#%x{console.log(#{@exp})}

			# the param for :dp0 is not used to instantiate the ditrib but set only to inform what is the paramEst[0] (used in AreaRisk)
			@deltaEst=[StatTestCurve.new(:dp0,[@paramEst[0]],false),StatTestCurve.new(:dp1,[@paramEst[1],0.15],false)]
			@plotDelta.addChild(@deltaEst[0]);@plotDelta.addChild(@deltaEst[1])
			@deltaEst[1].style[:fill]=%x{createjs.Graphics.getRGB(200,200,200,.3)};
			@deltaEst[1].style[:thickness]=1

			@context={}

			@paramLim=AcceptanceRegion.new(@paramEst[0],@context)
			@deltaLim=AcceptanceRegion.new(@deltaEst[0],@context)
			@paramLim.setPlot(@plotParam);@deltaLim.setPlot(@plotDelta)

			@paramTypeIRisk=AreaRisk.new(@paramEst[0],@context)
			@deltaTypeIRisk=AreaRisk.new(@deltaEst[0],@context)
			@paramTypeIRisk.setPlot(@plotParam);@deltaTypeIRisk.setPlot(@plotDelta)

			@paramTypeGenRisk=AreaRisk.new(@paramEst[1],@context)
			@paramTypeGenRisk.alpha=:paramH0
			@paramTypeGenRisk.setPlot(@plotParam)

			@deltaTypeGenRisk=AreaRisk.new(@deltaEst[1],@context)
			@deltaTypeGenRisk.alpha=:deltaH0
			@deltaTypeGenRisk.setPlot(@plotDelta)

			@paramEstLim=AcceptanceRegion.new(@paramEst[0],@context)
			@paramEstLim.alpha=:paramPval
			@deltaEstLim=AcceptanceRegion.new(@deltaEst[0],@context)
			@deltaEstLim.alpha=:deltaPval
			@paramEstLim.setPlot(@plotParam);@deltaEstLim.setPlot(@plotDelta)

			@paramPvalRisk=AreaRisk.new(@paramEst[0],@context)
			@paramPvalRisk.alpha=:paramEstLim
			@deltaPvalRisk=AreaRisk.new(@deltaEst[0],@context)
			@deltaPvalRisk.alpha=:deltaEstLim
			@paramPvalRisk.setPlot(@plotParam);@deltaPvalRisk.setPlot(@plotDelta)

			## styles
			@paramLim.style=@deltaLim.style=@styles[:lim]
			@paramEstLim.style=@deltaEstLim.style=@styles[:estLim]
			@paramPvalRisk.style=@deltaPvalRisk.style=@styles[:estLim]

			## movables
			@paramLim.initEvents
			@paramLim.addCallable([@paramTypeIRisk,:draw])
			@paramLim.addCallable([@paramTypeGenRisk,:draw])
			@paramLim.addCallable([@deltaLim,:draw])
			@paramLim.addCallable([@deltaTypeIRisk,:draw])
			@paramLim.addCallable([@deltaTypeGenRisk,:draw])

			@deltaLim.initEvents
			@deltaLim.addCallable([@deltaTypeIRisk,:draw])
			@deltaLim.addCallable([@deltaTypeGenRisk,:draw])
			@deltaLim.addCallable([@paramLim,:draw])
			@deltaLim.addCallable([@paramTypeIRisk,:draw])
			@deltaLim.addCallable([@paramTypeGenRisk,:draw])

			@paramEst[0].initEvents([:sd])
			@paramEst[0].addCallable([@paramEst[1],:paramsFrameAtFrom,[2,@paramEst[0]]],:sd)
			@paramEst[0].addCallable([@paramEst[1],:updateStatTestDistrib],:sd)
			@paramEst[0].addCallable([@paramEst[1],:draw],:sd)
			@paramEst[0].addCallable([@paramEst[1],:drawSD],:sd)
			@paramEst[0].addCallable([@paramLim,:draw],:sd)
			@paramEst[0].addCallable([@paramTypeIRisk,:draw],:sd)
			@paramEst[0].addCallable([@paramTypeGenRisk,:draw],:sd)
			@paramEst[0].addCallable([@deltaEst[1],:updateStatTestDistrib],:sd)
			@paramEst[0].addCallable([@deltaEst[1],:draw],:sd)
			@paramEst[0].addCallable([@deltaEst[1],:drawMean],:sd)
			@paramEst[0].addCallable([@deltaEst[1],:drawSD],:sd)
			@paramEst[0].addCallable([@deltaTypeGenRisk,:draw],:sd)
			@paramEst[0].addCallable([self,:setPval],:sd) #to recompute pval
			@paramEst[0].addCallable([@paramPvalRisk,:draw],:sd)


			@paramEst[1].initEvents
			@paramEst[1].addCallable([@paramTypeGenRisk,:draw])
			@paramEst[1].addCallable([@deltaEst[1],:updateStatTestDistrib])
			@paramEst[1].addCallable([@deltaEst[1],:draw])
			@paramEst[1].addCallable([@deltaEst[1],:drawMean])
			@paramEst[1].addCallable([@deltaEst[1],:drawSD])
			@paramEst[1].addCallable([@deltaTypeGenRisk,:draw])

			@paramEst[1].addCallable([@paramEst[0],:paramsFrameAtFrom,[2,@paramEst[1]]],:sd)
			@paramEst[1].addCallable([@paramEst[0],:updateStatTestDistrib],:sd)
			@paramEst[1].addCallable([@paramEst[0],:draw],:sd)
			@paramEst[1].addCallable([@paramEst[0],:drawSD],:sd)
			@paramEst[1].addCallable([@paramLim,:draw],:sd)
			@paramEst[1].addCallable([@paramTypeIRisk,:draw],:sd)
			@paramEst[1].addCallable([@paramTypeGenRisk,:draw],:sd)
			@paramEst[1].addCallable([@deltaEst[1],:updateStatTestDistrib],:sd)
			@paramEst[1].addCallable([@deltaEst[1],:draw],:sd)
			@paramEst[1].addCallable([@deltaEst[1],:drawMean],:sd)
			@paramEst[1].addCallable([@deltaEst[1],:drawSD],:sd)
			@paramEst[1].addCallable([@deltaTypeGenRisk,:draw],:sd)
			@paramEst[1].addCallable([self,:setPval],:sd) #to recompute pval
			@paramEst[1].addCallable([@paramPvalRisk,:draw],:sd)

			# attachment
			@paramLim.attachShapes;@deltaLim.attachShapes
			@paramTypeIRisk.attachShapes;@deltaTypeIRisk.attachShapes
			@paramTypeGenRisk.attachShapes;@deltaTypeGenRisk.attachShapes
			@paramEstLim.attachShapes;@deltaEstLim.attachShapes
			@paramPvalRisk.attachShapes;@deltaPvalRisk.attachShapes
			@paramEst[0].attachShapes;@paramEst[1].attachShapes
			@deltaEst[0].attachShapes;@deltaEst[1].attachShapes
			@plotParam.attachAxis;@plotDelta.attachAxis

			@n01=Distribution.new("normal",[0,1]);
			setAlpha(0.05)
			setStatMode(:none)

			reset

			# other init
			@style={fp:"#FFF",sp:"#000000",fl:"#FFF",sl:"#000000",fr:"rgba(100,100,255,0.8)",sr:"#000000"}


		end

		def setStyles
			@styles={} unless @styles
			@styles[:estLim]={fill: "rgba(240,130,40,.3)",stroke: "rgba(240,130,40,.8)"  ,thickness: 6}
			@styles[:known]={close: true, fill: "rgba(50,100,250,.1)",stroke: "rgba(50,150,250,.8)"  ,thickness: 3}
			@styles[:knownMovable]={close: true, fill: "rgba(50,100,250,.1)",stroke: "rgba(50,150,250,.8)"  ,thickness: 6}
			@styles[:unknown]={close: true, fill: "rgba(250,50,100,.1)",stroke: "rgba(250,50,100,.8)"  ,thickness: 3}
			@styles[:unknownMovable]={close: true, fill: "rgba(250,50,100,.1)",stroke: "rgba(250,50,100,.8)"  ,thickness: 6}
			@styles[:lim]={close: true, fill: "rgba(20,150,20,.1)",stroke: "rgba(20,150,20,.8)"  ,thickness: 6}
		end

		def setPval
			#left pval
			#p [:est,@context[:paramEstLim],@context[:deltaEstLim]]
			@context[:paramPval]=@context[:paramEstH0].distrib.cdf(@context[:paramEstLim])
			@context[:deltaPval]=@context[:deltaEstH0].distrib.cdf(@context[:deltaEstLim])
			#p [:pval,@context[:paramPval],@context[:deltaPval]]
			if @context[:side]==">"
				@context[:paramPval]=1-@context[:paramPval]
				@context[:deltaPval]=1-@context[:deltaPval]
			elsif @context[:side]=="!="
				@context[:paramPval]=2*[@context[:paramPval],1-@context[:paramPval]].min
				@context[:deltaPval]=2*[@context[:deltaPval],1-@context[:deltaPval]].min
			end
			#p [:pval,@context[:paramPval],@context[:deltaPval]]

		end

		def getContext
			@context[:param]= %x{cqlsHypo.f.getValue('param')}
			@context[:paramValue]= %x{parseFloat(cqlsHypo.f.getValue('paramValue'))}
			@context[:side]=%x{cqlsHypo.f.getValue('side')}
			@context[:ref]=%x{parseFloat(cqlsHypo.f.getValue('refValue'))}
			@context[:n]=%x{parseInt(cqlsHypo.f.getValue('nValue'))}
			@context[:alpha]=@alpha
			@context[:sigma]=1

			%x{console.log('param:' + #{@context[:param]})}

			case @context[:param]
			when "p"
				@paramEst[0].paramsFrame=[@context[:n],@context[:ref]]
				@paramEst[1].paramsFrame=[@context[:n],(@context[:paramValue] ? @context[:paramValue].to_f :  @context[:ref].to_f*(@context[:side]=="<" ? 0.5 : 1.5))]
				@context[:paramEstLim]=%x{parseFloat(cqlsHypo.f.getValue('meanValue'))}
				@context[:deltaEstLim]=(@context[:paramEstLim]-@context[:ref])/(%x{Math.sqrt(#{@context[:ref]*(1-@context[:ref])/@context[:n]})})
			when "mu"
				@context[:param]="m"
				@paramEst[0].paramsFrame=[@context[:n],@context[:ref],@context[:sigma]]
				@paramEst[1].paramsFrame=[@context[:n],(@context[:paramValue] ? @context[:paramValue].to_f :  @context[:ref].to_f*(@context[:side]=="<" ? 0.5 : 1.5)),@context[:sigma]]
				@context[:paramEstLim]=%x{parseFloat(cqlsHypo.f.getValue('meanValue'))}
				sd=%x{parseFloat(cqlsHypo.f.getValue('sdValue'))}
				@context[:deltaEstLim]=(@context[:paramEstLim]-@context[:ref])/(sd/%x{Math.sqrt(#{@context[:n]})})
			end

			## Be carefull @context[:param] modified inside case
			@paramEst[0].typeStatTest=@context[:param]
			@paramEst[1].typeStatTest=@context[:param]
			@context[:paramEstH0]=@paramEst[0]
			@context[:deltaEstH0]=@deltaEst[0]

			@deltaEst[0].typeStatTest="d"+@context[:param]+"0"
			@deltaEst[1].typeStatTest="d"+@context[:param]+"1"


			@paramEst[1].updateStatTestDistrib
			@paramEst[0].updateStatTestDistrib
			@deltaEst[1].updateStatTestDistrib

			setPval

			case @context[:param]
			when :p
				@paramEst[0].style=@styles[:known]
				@paramEst[0].meanStyle=@styles[:known]
				@paramEst[0].sdStyle=@styles[:known]
				@paramEst[1].style=@styles[:unknown]
				@paramEst[1].meanStyle=@styles[:unknownMovable]
				@paramEst[1].sdStyle=@styles[:unknownMovable]
				@deltaEst[0].style=@styles[:known]
				@deltaEst[0].meanStyle=@styles[:known]
				@deltaEst[0].sdStyle=@styles[:known]
				@deltaEst[1].style=@styles[:unknown]
				@deltaEst[1].meanStyle=@styles[:unknown]
				@deltaEst[1].sdStyle=@styles[:unknown]
			when :m
				@paramEst[0].style=@styles[:unknown]
				@paramEst[0].meanStyle=@styles[:known]
				@paramEst[0].sdStyle=@styles[:unknownMovable]
				@paramEst[1].style=@styles[:unknown]
				@paramEst[1].meanStyle=@styles[:unknownMovable]
				@paramEst[1].sdStyle=@styles[:unknownMovable]
				@deltaEst[0].style=@styles[:known]
				@deltaEst[0].meanStyle=@styles[:known]
				@deltaEst[0].sdStyle=@styles[:known]
				@deltaEst[1].style=@styles[:unknown]
				@deltaEst[1].meanStyle=@styles[:unknown]
				@deltaEst[1].sdStyle=@styles[:unknown]
			end
		end

		def reset(curs=[0,1])

			getContext

			@graphParam.active=["curve0","curve1"]
			@graphParam.update
			@plotParam.update

			@graphDelta.active=["curve2","curve3"]
			@graphDelta.update
			@plotDelta.update

			#####@graphHist.update
			curs.each do |cur|
				@paramEst[cur].draw
				@deltaEst[cur].draw
			end

			updateVisible
	    end

		def setStatMode(mode)
			@statMode=(mode==:meanIC ? :ic : :none)
			#p [:setStatMode,mode,@statMode]
		end

		def setAlpha(alpha)
			@alpha=alpha
		end


		def updateVisible #from interface

			%x{
				#{@paramEst[0]}.shape.visible=cqlsHypo.f.getValue('checkParam0Curve');
				#{@paramEst[1]}.shape.visible=cqlsHypo.f.getValue('checkParam1Curve');
				#{@deltaEst[0]}.shape.visible=cqlsHypo.f.getValue('checkDelta0Curve');
				#{@deltaEst[1]}.shape.visible=cqlsHypo.f.getValue('checkDelta1Curve');


				// Lim
				#{@paramLim}.shapes[0].visible= cqlsHypo.f.getValue('checkParamLim');
				#{@paramLim}.shapes[1].visible= cqlsHypo.f.getValue('checkParamLim');
				#{@deltaLim}.shapes[0].visible= cqlsHypo.f.getValue('checkDeltaLim');
				#{@deltaLim}.shapes[1].visible= cqlsHypo.f.getValue('checkDeltaLim');
				#{@paramEstLim}.shapes[0].visible= cqlsHypo.f.getValue('checkData');
				#{@paramEstLim}.shapes[1].visible= cqlsHypo.f.getValue('checkData');
				#{@deltaEstLim}.shapes[0].visible= cqlsHypo.f.getValue('checkData') & (cqlsHypo.f.getValue('checkDelta0Mean') | cqlsHypo.f.getValue('checkDeltaLim'));
				#{@deltaEstLim}.shapes[1].visible= cqlsHypo.f.getValue('checkData') & (cqlsHypo.f.getValue('checkDelta0Mean') | cqlsHypo.f.getValue('checkDeltaLim'));

				//Risk
				#{@paramTypeIRisk}.shapes[0].visible= cqlsHypo.f.getValue('checkRiskTypeI') & cqlsHypo.f.getValue('checkParam0Curve');
				#{@paramTypeIRisk}.shapes[1].visible= cqlsHypo.f.getValue('checkRiskTypeI') & cqlsHypo.f.getValue('checkParam0Curve');
				#{@deltaTypeIRisk}.shapes[0].visible= cqlsHypo.f.getValue('checkRiskTypeI') & cqlsHypo.f.getValue('checkDelta0Curve');
				#{@deltaTypeIRisk}.shapes[1].visible= cqlsHypo.f.getValue('checkRiskTypeI') & cqlsHypo.f.getValue('checkDelta0Curve');

				#{@paramTypeGenRisk}.shapes[0].visible= cqlsHypo.f.getValue('checkRiskTypeGen') & cqlsHypo.f.getValue('checkParam1Curve');
				#{@paramTypeGenRisk}.shapes[1].visible= cqlsHypo.f.getValue('checkRiskTypeGen') & cqlsHypo.f.getValue('checkParam1Curve');
				#{@deltaTypeGenRisk}.shapes[0].visible= cqlsHypo.f.getValue('checkRiskTypeGen') & cqlsHypo.f.getValue('checkDelta1Curve');
				#{@deltaTypeGenRisk}.shapes[1].visible= cqlsHypo.f.getValue('checkRiskTypeGen') & cqlsHypo.f.getValue('checkDelta1Curve');

				#{@paramPvalRisk}.shapes[0].visible= cqlsHypo.f.getValue('checkPval') & cqlsHypo.f.getValue('checkParam0Curve');
				#{@paramPvalRisk}.shapes[1].visible= cqlsHypo.f.getValue('checkPval') & cqlsHypo.f.getValue('checkParam0Curve');
				#{@deltaPvalRisk}.shapes[0].visible= cqlsHypo.f.getValue('checkPval') & cqlsHypo.f.getValue('checkDelta0Curve');
				#{@deltaPvalRisk}.shapes[1].visible= cqlsHypo.f.getValue('checkPval') & cqlsHypo.f.getValue('checkDelta0Curve');


				#{@paramEst[0]}.summaryShapes[0].visible=cqlsHypo.f.getValue('checkParam0Mean');
				#{@paramEst[0]}.summaryShapes[1].visible=cqlsHypo.f.getValue('checkParam0SD');
				#{@paramEst[1]}.summaryShapes[0].visible=cqlsHypo.f.getValue('checkParam1Mean');
				#{@paramEst[1]}.summaryShapes[1].visible=cqlsHypo.f.getValue('checkParam1SD');

				#{@deltaEst[0]}.summaryShapes[0].visible=cqlsHypo.f.getValue('checkDelta0Mean');
				#{@deltaEst[0]}.summaryShapes[1].visible=cqlsHypo.f.getValue('checkDelta0SD');
				#{@deltaEst[1]}.summaryShapes[0].visible=cqlsHypo.f.getValue('checkDelta1Mean');
				#{@deltaEst[1]}.summaryShapes[1].visible=cqlsHypo.f.getValue('checkDelta1SD');

				// update stage since possible change of visibility
				cqlsHypo.m.stage.update();
			}
		end

	end

	class Distribution

		attr_accessor :list, :name, :params, :distrib

		def initialize(name=nil,params=[],transf=nil)
			unless @@list
				@@list={
					uniform: {
						type: :cont,
						dist: ["UniformDistribution"],
						qbounds: [0,1]
					},
					normal: {
						type: :cont,
						dist: ["NormalDistribution"],
						qbounds: [%x{cqlsHypo.m.qmin},%x{cqlsHypo.m.qmax}]
					},
					t: {
						type: :cont,
						dist:["StudentDistribution"],
						qbounds: [%x{cqlsHypo.m.qmin},%x{cqlsHypo.m.qmax}]
					},
					chi2: {
						type: :cont,
						dist: ["ChiSquareDistribution"],
						qbounds: [0,%x{cqlsHypo.m.qmax}]
					},
					exp: {
						type: :cont,
						dist: ["ExponentialDistribution"],
						qbounds: [0,%x{cqlsHypo.m.qmax}]
					},
					cauchy: {
						type: :cont,
						dist: ["CauchyDistribution"],
						qbounds: [0.01,0.99]
					},
					discreteUniform: {
						type: :disc,
						dist: ["DiscreteUniformDistribution"],
						qbounds: [0,1]
					},
					bernoulli: {
						type: :disc,
						dist: ["BernoulliDistribution"],
						qbounds: [0,1]
					},
					binomial: {
						type: :disc,
						dist: ["BinomialDistribution"],
						qbounds: [0,1]
					},
					birthday: {
						type: :disc,
						dist: ["BirthdayDistribution"],
						qbounds: [0.01,1]
					},
					mean: {
						dist: :none,
						qbounds: [0,1]
					},
					sum: {
						dist: :none,
						qbounds: [0,1]
					},
					locationScale: {
						dist: :none,
						qbounds: [0,1]
					},
					square: {
						dist: :none,
						qbounds: [0,1]
					},
					sumOfSq: {
						dist: :none,
						qbounds: [0,1]
					}
				}
			end
			@list=@@list
			if name
				if transf
					setAsTransfOf(Distribution.new(name,params),transf)
				else
					set(name,params)
				end
			end
		end

		def set(dist,params)
			@name,@params=dist,params
			#p [@name,@list[@name]]
			@type=@list[@name][:type]
			instr="new "+@list[@name]["dist"].join(".")+"("+@params.join(',')+");"
			@distrib=%x{eval(#{instr})}
		end

		def setAsTransfOf(dist,transf) #dist is here of Distribution (Opal) class
			@name,@params=transf[:name],transf[:args]
			@originalDistrib=dist
			case @name
			when :square
				@distrib=%x{new PowerDistribution(#{@originalDistrib}.distrib,2)}
			when :mean
				d=%x{new Convolution(#{@originalDistrib}.distrib,#{@params[0]})}
				@distrib=%x{new LocationScaleDistribution(#{d},0,1/#{@params[0]})}
			when :sum
				@distrib=%x{new Convolution(#{@originalDistrib}.distrib,#{@params[0]})}
			when :locationScale
				@distrib=%x{new LocationScaleDistribution(#{@originalDistrib}.distrib,#{@params[0]},#{@params[1]})}
			when :sumOfSq
				d=%x{new LocationScaleDistribution(#{@originalDistrib}.distrib,-#{@originalDistrib.mean}/#{@originalDistrib.stdDev},1/#{@originalDistrib.stdDev})}
				d=%x{new PowerDistribution(#{d},2)}
				if %x{d.type === CONT}
					@distrib=%x{new Convolution(#{d},#{@params[0]})}
				else
					@distrib=Convolution.power(d,@params[0])
					p [:boundsDistrib,step,bounds,pdf(bounds),pdf(bounds).inject(0) {|e,e2| e += e2}]

				end
			end
			# p [:type,type]
		end

		def type
			@type || @originalDistrib.type
		end

		def qbounds
			@list[@name][:qbounds]
		end

		def bounds
			qb=@originalDistrib ? @originalDistrib.qbounds : qbounds
			case type
			when :cont
				qb.map{|e| quantile(e)}
			when :disc
				if regular?
					#p qb;%x{console.log(#{@distrib})}
					a,b=qb.map{|e| quantile(e)}
					s=step
					# p [:bounds,a,b,s,Range.new(0,((b-a)/s)).to_a.map{|e| a+e*s}]
					Range.new(0,((b-a)/s)).to_a.map{|e| a+e*s}
				else
					%x{#{@distrib}.values()} #this is the tricks for nonequidistant discrete rv.
				end
			end
		end

		def minValue
			%x{#{@distrib}.minValue()}
		end

		def maxValue
			%x{#{@distrib}.maxValue()}
		end

		def regular?
			%x{#{@distrib}.regular()}
		end

		def step
			if regular?
				%x{#{@distrib}.step()}
			else
				b=bounds
				(1...b.length).map {|i| (b[i]-b[i-1]).abs}.min.to_f
			end
		end

		def mean
			%x{#{@distrib}.mean()}
		end

		def mode
			%x{#{@distrib}.mode()}
		end

		def maxPdf
			%x{#{@distrib}.maxDensity()}
		end

		def variance
			%x{#{@distrib}.variance()}
		end

		def stdDev
			%x{#{@distrib}.stdDev()}
		end

		def sample(n=1)
			%x{z=[];for(i=0;i<#{n};i++) z[i]=#{@distrib}.simulate();return z}
		end

		def pdf(x)
			%x{#{x}.map(function(e) {return #{@distrib}.density(e);})}
		end

		def cdf(x)
			%x{#{@distrib}.CDF(#{x})}
		end

		def quantile(alpha)
			%x{#{@distrib}.quantile(#{alpha})}
		end
	end

	class Convolution

		def Convolution.power(d,n)
			if %x{#{d} instanceof Distribution}
				dist,b=d,%x{d.values()}
				dist2,b2=d,%x{d.values()}
			else
				dist,b=d.distrib,d.bounds
				dist2,b2=d.distrib,d.bounds
			end
			# %x{console.log(["convN",b,b2,dist,dist2])}
			(1...n).each do |i|
				dist2=%x{new Convolution2(#{dist},#{dist2},#{b},#{b2})}
				b2=%x{dist2.values()}
			end
			#p ["ici",b2]
			# %x{console.log(#{dist2})}
			dist2
		end

		def Convolution.two(d,d2)
			dist,b=d.distrib,d.bounds
			dist2,b2=d2.distrib,d2.bounds
			%x{new Convolution2(#{dist},#{dist2},#{b},#{b2})}
		end



		## used inside distributions.js to prepare all the stuff
		def initialize(d1,d2,b1,b2)
			@d1,@d2,@b1,@b2=d1,d2,b1,b2
			# %x{console.log(#{["init",@d1,@d2,@b1,@b2]})}
			prepare
		end

		def prepare
			ind={}
			#p ["prep0",@b1,@b2]
			@b1.each_with_index {|v1,i1|
				@b2.each_with_index{|v2,i2|
					v=CqlsHypo.quantize(v1+v2)
					#p ["prep",v1+v2,v]
					if ind.keys.include? v
						ind[v] << [i1,i2]
					else
						ind[v]=[[i1,i2]]
					end
				}
			}
			@bounds=ind.keys.sort
			@pdf=[]
			@bounds.each_with_index {|v,i|
				@pdf[i]=0
				ind[v].each {|j1,j2|
					#p [j1,j2,@b1[j1],@b2[j2],%x{#{@d1}.density(#{@b1[j1]})},%x{#{@d2}.density(#{@b2[j2]})}]
					@pdf[i] += %x{#{@d1}.density(#{@b1[j1]})* #{@d2}.density(#{@b2[j2]})}
				}
			}
			#p [@bounds,@pdf]
		end

	end

	PREC4DISC=0

	def CqlsHypo.quantize(x,prec=PREC4DISC)
		%x{parseFloat(#{x}.toFixed(#{prec}))}
	end

	def CqlsHypo.equal(a,b)
		%x{a.toFixed(#{PREC4DISC})===b.toFixed(#{PREC4DISC})}
	end

	def CqlsHypo.range(low, high, step) # from http://phpjs.org/functions/range/
		%x{
			// From: http://phpjs.org/functions
			// +   original by: Waldo Malqui Silva
			// *     example 1: range ( 0, 12 );
			// *     returns 1: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
			// *     example 2: range( 0, 100, 10 );
			// *     returns 2: [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
			// *     example 3: range( 'a', 'i' );
			// *     returns 3: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']
			// *     example 4: range( 'c', 'a' );
			// *     returns 4: ['c', 'b', 'a']
			var matrix = [];
			var inival, endval, plus;
			var walker = step || 1;
			var chars = false;

			if (!isNaN(low) && !isNaN(high)) {
			inival = low;
			endval = high;
			} else if (isNaN(low) && isNaN(high)) {
			chars = true;
			inival = low.charCodeAt(0);
			endval = high.charCodeAt(0);
			} else {
			inival = (isNaN(low) ? 0 : low);
			endval = (isNaN(high) ? 0 : high);
			}

			plus = ((inival > endval) ? false : true);
			if (plus) {
			while (inival <= endval) {
			  matrix.push(((chars) ? String.fromCharCode(inival) : inival));
			  inival += walker;
			}
			} else {
			while (inival >= endval) {
			  matrix.push(((chars) ? String.fromCharCode(inival) : inival));
			  inival -= walker;
			}
			}

			return matrix;
		}
	end

	def CqlsHypo.seq(min, max, length) #from jStat
		%x{
			var arr = [],
			hival = Math.pow(10, 17 - ~~(Math.log(((max > 0) ? max : -max)) * Math.LOG10E)),
			step = (max * hival - min * hival) / ((length - 1) * hival),
			current = min,
			cnt = 0;
			// current is assigned using a technique to compensate for IEEE error
			for (; current <= max; cnt++, current = (min * hival + step * hival * cnt) / hival)
				arr.push(current);
			return arr;
		}
	end


end
