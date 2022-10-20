module CqlsAEP

	class Plot

		attr_accessor :parent, :frame, :style, :graph, :dim

		def initialize(dim={x:0,y:0,w:%x{cqlsAEP.i.dim.w},h:%x{cqlsAEP.i.dim.h}},style={bg:"#88FF88"})
			@dim,@style=dim,style
			@parent=%x{new createjs.Container()}
			@frame=	%x{new createjs.Shape()}
    		@graph=CqlsAEP::Graph.new(@dim)
    		@updateCalls=[]
    		# init frame
    		%x{#{@frame}.graphics.beginLinearGradientFill(["#FFF",#{@style[:bg]}], [0, 1], 0, #{@dim[:y]}+20, 0, #{@dim[:y]}+#{@dim[:h]}+20).drawRect(#{@dim[:x]},#{@dim[:y]},#{@dim[:w]},#{@dim[:h]})}
			addChild(@frame)
			@axisShape=%x{new createjs.Shape()}
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
    		if pos>=0
    			%x{#{@parent}.addChildAt(#{shape},#{pos})}
    		else
    			%x{#{@parent}.addChild(#{shape})}
    		end
    	end

    	# def removeChild(child)
    	# 	@updateCalls.delete(child)
    	# end

    	def update
    		##p ["update",self]
    		@updateCalls.each do |k,v|
    			##p v
    			args=v[2]
    			args=[] unless args
    			v[0].method(v[1]).call(*args)
    		end
    		##p ["update2",self]
    	end

    	def drawAxis
    		%x{#{@axisShape}.graphics.ss(3,2).s("#000").mt(#{@dim[:x]},#{@graph.to_Y(0.0)}).lt(#{@dim[:x]+@dim[:w]},#{@graph.to_Y(0.0)}).es()}
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
			unless synced?
			#%x{console.log(#{@list})};p @dim;
			#p active
				list=@list.select{|e| active.empty? or (active.include? e[1])}
				#%x{console.log(#{list})}
				@xylim[:x][0]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:x][0]}.min
				@xylim[:x][1]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:x][1]}.max
				@xylim[:y][0]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:y][0]}.min
				@xylim[:y][1]=list.map{|e| e2=(e[0]==:element ? e[2].xylim : e[2]);e2[:y][1]}.max
			end
			# then update the rescaling coeff
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

			updateZoom(select)

			return select
		end

		def updateZoom(mode,times=1)
			step=0.1/2
			(0...times).each do
				case mode
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
			end

		end

	end

	class Child

		attr_accessor :id, :plot, :graph, :shape, :style, :xylim

		def initialize

		end

		# def plot=(plot)
		# 	@graph=plot.graph
		# end

	end

	## This behaves as an Experiment
	## since it responds to xy method
	class Curve < Child

		attr_accessor :distrib, :bounds, :kind, :type, :style

		def initialize(id=nil,type=:cont,bounds=[0,1],style={close: true,stroke:"#000",fill:"rgba(200,200,255,0.5)",thickness: 3},length=512)
			@@curveCpt=-1 unless @@curveCpt
			@id=id || "curve"+(@@curveCpt+=1).to_s
			@type=type
			case @type
			when :cont
				@bounds,@length=bounds,length
			when :disc
				@bounds=bounds #sequence of ordered values
				@length=@bounds.length #fortunately useless
				initStep
			end
			@style=style
			@shape=%x{new createjs.Shape()}
			@x=CqlsAEP.seq(@bounds[0],@bounds[1],@length)
			@kind=:density
			@summaryShapes=[%x{new createjs.Shape()},%x{new createjs.Shape()}]
			@expAxisShape=%x{new createjs.Shape()}
		end

		def attachExpAxis(ratio)
			@plot.addChild(@expAxisShape,[self,:drawExpAxis,[ratio]])
		end

		def drawExpAxis(ratio)
			%x{#{@expAxisShape}.visible=true}
			%x{#{@expAxisShape}.graphics.c().s("#000").ss(1).mt(#{@graph.dim[:x]},#{@graph.dim[:h]*ratio}).lt(#{@graph.dim[:x]+@graph.dim[:w]},#{@graph.dim[:h]*ratio})}
		end

		def attachSummary
			@plot.addChild(@summaryShapes[0],[self,:drawMean])
			@plot.addChild(@summaryShapes[1],[self,:drawSD])
		end

		def drawMean
			# %x{#{@summaryShapes[0]}.visible=true}
			%x{#{@summaryShapes[0]}.graphics.c().s("#000").ss(1).mt(#{@graph.to_X(@distrib.mean)},#{@graph.dim[:y]}).lt(#{@graph.to_X(@distrib.mean)},#{@graph.dim[:y]+@graph.dim[:h]})}
		end

		def drawSD
			x,y=10,10
			h=@distrib.maxPdf/2.0
			h=h/@step if @type==:disc
			h=@graph.to_Y(h)
			# %x{#{@summaryShapes[1]}.visible=true}
			%x{
				#{@summaryShapes[1]}.graphics.c().s("#000").ss(1)
				.mt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)}+x,#{h}-y)
				.lt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)},#{h})
				.lt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)}+x,#{h}+y)
				.mt(#{@graph.to_X(@distrib.mean-@distrib.stdDev)},#{h})
				.lt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)},#{h})
				.lt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)}-x,#{h}-y)
				.mt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)},#{h})
				.lt(#{@graph.to_X(@distrib.mean+@distrib.stdDev)}-x,#{h}+y)
			}
		end

		#######################################
		## This is required for an experiment
		def sample(n=1)
			###p [:sample,n,@distrib.name,@distrib,@distrib.sample(n)]
			@distrib.sample(n)
		end

		def y(x)
			y=@distrib.pdf(x)
			y.map!{|e| e/@distrib.step} if @distrib.type==:disc
			y=%x{#{y}.map(function(e) {return Math.random()*e;})}
			y
		end

		def xy(n=1)
			###p [:xy, sample(n)]
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
				@x=CqlsAEP.seq(@bounds[0],@bounds[1],@length)
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
			%x{#{shape}.graphics.mt(#{graph.to_X(@x[0])},#{graph.to_Y(0.0)})}
			# Carefull, @x.length may differ from @length => do not use @length below
			(0...@x.length).each {|i|
				#p [i,"a",@x[i],@y[i]]
				#p ["b",graph.to_X(@x[i]),graph.to_Y(@y[i])]
				%x{#{shape}.graphics.lt(#{graph.to_X(@x[i])},#{graph.to_Y(@y[i])})}
			}
			%x{#{shape}.graphics.lt(#{graph.to_X(@x[-1])},#{graph.to_Y(0.0)})}
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

	end

	class Hist < Child

		attr_accessor :bounds, :level, :levels, :nbPart, :nbTot, :curveShape, :type, :aep, :style, :cptICTot

		def initialize(id=nil,type=:cont,bounds=[0,1],style={hist: {fill:"rgba(100,100,255,0.5)",stroke:"#000000"},mean: {stroke: "rgba(100,100,255,1)",thickness: 1},sd: {stroke: "rgba(100,100,255,1)",thickness: 1,fill:"rgba(100,100,255,1)" },curve:{close:false,fill:"#000",stroke:"rgba(0,0,0,.4)",thickness:3}},levels=8)
			@@histCpt=-1 unless @@histCpt
			@id=id || "hist"+(@@histCpt+=1).to_s
			@type=type
			case @type
			when :cont
				@bounds,@levels,@level=bounds,levels,4
				@nbPart=2**levels
			when :disc
				@bounds=bounds
			end
			init
			@style=style
			@shape=%x{new createjs.Shape()}
			@curveShape=%x{new createjs.Shape()}
			@aep={}
			@aepLastStep={}
			@summaryShapes=[%x{new createjs.Shape()},%x{new createjs.Shape()}]
		end

		def drawCurve
			@curve.draw(@curveShape,@graph,@style[:curve])
		end

		def attachSummary
			@plot.addChild(@summaryShapes[0],[self,:drawMean])
			@plot.addChild(@summaryShapes[1],[self,:drawSD])
		end

		def drawMean
			# %x{#{@summaryShapes[0]}.visible=#{@nbTot>0}}
			%x{#{@summaryShapes[0]}.graphics.c().s(#{@style[:mean][:stroke]}).ss(#{@style[:mean][:thickness]}).mt(#{@graph.to_X(@mean[0])},#{@graph.dim[:y]}).lt(#{@graph.to_X(@mean[0])},#{@graph.dim[:y]+@graph.dim[:h]})}
		end

		def drawSD
			x,y=10,10
			#p [@curve.distrib.maxPdf,@mean[0],@sd]
			h=@curve.distrib.maxPdf/2.0
			h=h/@step if @type==:disc
			h=@graph.to_Y(h)
			#%x{#{@summaryShapes[1]}.visible=#{@nbTot>0}}
			%x{
				#{@summaryShapes[1]}.graphics.c().s(#{@style[:mean][:stroke]}).ss(1)
				.mt(#{@graph.to_X(@mean[0]-@sd)}+x,#{h}-y)
				.lt(#{@graph.to_X(@mean[0]-@sd)},#{h})
				.lt(#{@graph.to_X(@mean[0]-@sd)}+x,#{h}+y)
				.mt(#{@graph.to_X(@mean[0]-@sd)},#{h})
				.lt(#{@graph.to_X(@mean[0]+@sd)},#{h})
				.lt(#{@graph.to_X(@mean[0]+@sd)}-x,#{h}-y)
				.mt(#{@graph.to_X(@mean[0]+@sd)},#{h})
				.lt(#{@graph.to_X(@mean[0]+@sd)}-x,#{h}+y)
			}
		end

		def updateBounds
			@bounds=@curve.bounds
			#p ["updateBounds",@bounds]
		end

		def regular?
			@curve.regular?
		end

		# possible type: auto_cont, auto_disc
		def init
			case @type
			when :cont
				@step=(@bounds[1]-@bounds[0]).to_f/@nbPart
				@cpt,@nbTot=[0]*@nbPart,0
				@outside=[0]*2
			when :disc
				@step=(1...@bounds.length).map {|i| (@bounds[i]-@bounds[i-1]).abs}.min
				@cpt,@nbTot=[0]*@bounds.length,0
				@outside=[0]*2
				unless regular?
					@ind={};@bounds.each_with_index{|v,i| @ind[v]=i}
				end
				## index to convert value to its rank starting from 0
				## This is removed from now since only useful for non-equidistant modalities
				## @ind={};@bounds.each_with_index{|v,i| @ind[v]=i}
				 # p [:bounds,@bounds]
				 # #p [:ind,@ind]
				 # p [:cpt,@cpt]
			end
			@mean,@sd,@cptICTot=[0,0],0,0
		end

		def index(x,step=@step)
			regular? ? ((x-@bounds[0])/step).floor : @ind[CqlsAEP.quantize(x)]
			# TODO: @ind[x] when considering non-equidistant modalities
		end

		def reset(type=nil)
			@type=type ? type : @curve.type
			updateBounds
			init
		end

		def attachCurve(curve)
			@curve=curve
			## logic
			reset
			## shape
			#@xylim=curve.xylim
			@graph.add(@curve)
			drawCurve
			@plot.addChild(@curveShape,[self,:drawCurve],1)
			%x{#{@curveShape}.visible=false}
		end

		def add(x)
			case @type
			when :cont
				x.each {|e|
					if @bounds[0] <= e and e <= @bounds[-1]
						@cpt[index(e)]+=1
					else
						@outside[0] += 1 if e < @bounds[0]
						@outside[1] += 1 if e > @bounds[-1]
					end
				}
			when :disc
				x.each {|e|
					i=index(e)
					if 0 <= i and i < @bounds.length
						@cpt[i]+=1
					else
						@outside[0] += 1 if i < 0
						@outside[1] += 1 if i >= @bounds.length
					end
				}
			end
			@mean[0]= x.inject(@nbTot.to_f*@mean[0]) {|e,e2| e+=e2}
			@mean[1]= x.inject(@nbTot.to_f*@mean[1]) {|e,e2| e+=e2**2}
			@nbTot += x.length
			@mean[0]=@mean[0]/@nbTot.to_f
			@mean[1]=@mean[1]/@nbTot.to_f
			@sd=%x{Math.sqrt(#{@mean[1]-@mean[0]**2})}
			##p [@mean,@mean[1]-@mean[0]**2]
		end

		def incCptIC(cpt)
			@cptICTot += cpt
		end

		def level(val=0,mode=:inc) #or :assign
			return nil if @type==:disc
			return @levelNext if mode==:inc and val==0
			level = (mode==:inc ? @levelNext : 0) + val
			level=0 if level < 0
			level=@levels if level > @levels
			@levelNext = level
			acceptLevelNext
			return @levelNext
		end

		def acceptLevelNext
			@level=@levelNext if %x{cqlsAEP.i.allowLevelChange}
		end

		def counts
			return @cpt.dup if @type==:disc
			cptLevel=[0]*(2**@level)
			(0...@nbPart).each{|i| cptLevel[i / 2**(@levels-@level)] += @cpt[i]}
			cptLevel
		end

		def prob
			counts().map{|e| e.to_f/@nbTot.to_f}
		end

		def density(nbTot=nil)
			cpt=counts
			#p [:cpt,cpt]
			step=(@type==:cont ? @step*(2**(@levels-@level)) : @step)
			nbTotal = nbTot ? nbTot : @nbTot
			#p [nbTotal,step]
			cpt.map{|e| e.to_f/nbTotal.to_f/step}
		end

		def partBounds
			case @type
			when :cont
				step=@step*(2**(@levels-@level))
				((0...(2**@level)).map{|i| @bounds[0]+i*step})+[@bounds[1]]
			when :disc
				s=@step/2.0
				(@bounds.map{|v| v-s})+[@bounds[-1]+s]
			end
		end

		def draw(nbTot=nil)
			d=density(nbTot)
			b=partBounds()
			#p d;p b
			l=@type==:cont ? 2**@level : @bounds.length
			%x{#{@shape}.graphics.c().f(#{@style[:hist][:fill]}).s(#{@style[:hist][:stroke]}).mt(#{@graph.to_X(b[0])},#{@graph.to_Y(0.0)})}
			(0...(l)).each {|i|
				%x{
					if(#{@type==:disc}) {
						#{@shape}.graphics.lt(#{@graph.to_X(b[i])},#{@graph.to_Y(0)});
					}
					#{@shape}.graphics.lt(#{@graph.to_X(b[i])},#{@graph.to_Y(d[i])});
					if(#{regular?}) {
						#{@shape}.graphics.lt(#{@graph.to_X(b[i+1])},#{@graph.to_Y(d[i])});
						if(#{@type==:disc}) {
							#{@shape}.graphics.lt(#{@graph.to_X(b[i+1])},#{@graph.to_Y(0)});
						}
					}
					else {//then implicitly discrete
						#{@shape}.graphics.lt(#{@graph.to_X(b[i]+@step)},#{@graph.to_Y(d[i])});
						#{@shape}.graphics.lt(#{@graph.to_X(b[i]+@step)},#{@graph.to_Y(0)});
					}
				}
			}
			%x{#{@shape}.graphics.lt(#{@graph.to_X(b[-1])},#{@graph.to_Y(0.0)})}
			%x{#{@shape}.graphics.cp()}
		end

		def updateHistAEP(x,mode=:normal) #,info={cpt: [],step: 0, nbTot: 0, xRect: [],  })
			## made in the current level
			@aep[:cpt]=(mode==:normal ? counts : ([0]*(@type==:disc ? @bounds.length : (2**@level)) )) #mode==:new
			@aep[:step]= @type==:cont ? (@bounds[1]-@bounds[0]).to_f / (2**@level).to_f : @step
			@aep[:nbTot] = (([:normal,:reduced].include? mode) ? @nbTot : 0) + x.length
			@aep[:xRect],@aep[:yRect]=[],[]

			case @type
			when :cont
				x.each_with_index {|e,i|
					if @bounds[0] <= e and e <= @bounds[-1]
						pos=((e-@bounds[0]) / (@aep[:step]) ).floor
						@aep[:xRect][i]=@bounds[0]+(@aep[:step]*pos.to_f)
						@aep[:cpt][pos] += 1
						@aep[:yRect][i]=@aep[:cpt][pos].to_f/@aep[:nbTot].to_f/@aep[:step]
					else
						pos=((e-@bounds[0]) / (@aep[:step]) ).floor
						@aep[:xRect][i]=@bounds[0]+(@aep[:step]*pos.to_f)
						@aep[:yRect][i]=0
					end
				}
			when :disc
				x.each_with_index {|e,i|
					pos=index(e)
					if 0 <= pos and pos < @bounds.length
						@aep[:xRect][i]=@bounds[pos]-@aep[:step]/2.0
						@aep[:cpt][pos] += 1
						@aep[:yRect][i]=@aep[:cpt][pos].to_f/@aep[:nbTot].to_f/@aep[:step]
					else
						@aep[:xRect][i]=e-@aep[:step]/2.0
						@aep[:yRect][i]=0
					end
				}
			end

			if mode==:new
				@aep[:mean]=[
					(x.inject(0) {|e,e2| e+=e2})/@aep[:nbTot],
					(x.inject(0) {|e,e2| e+=e2**2})/@aep[:nbTot]
				]
				@aep[:sd]=%x{Math.sqrt(#{@aep[:mean][1]-@aep[:mean][0]**2})}
			end
		end

	end

	class Play

		attr_accessor :exp

		def initialize(plotExp=%x{cqlsAEP.s.plot},plotHist=%x{cqlsAEP.h.plot})
			## Since animation is made in javascript
			## Anim objects are declared in javascript
			## Generally, there is only one AEPActors too!
			%x{
				cqlsAEP.actors={pt:[],rect:[],line:[]};
				cqlsAEP.tweens={pt:[],rect:[],line:[]};
	    		cqlsAEP.m.nbsSimMax=cqlsAEP.m.nbsSim["1000"][cqlsAEP.m.nbsSim["1000"].length-1];
	    		//console.log("nbsSImMax="+cqlsAEP.m.nbsSimMax);
	     		for(i=0;i<cqlsAEP.m.nbsSimMax;i++) {
					var rect=new createjs.Shape();
	    			cqlsAEP.actors.rect.push(rect);
			    	rect.visible=false;
			    	cqlsAEP.m.stage.addChild(rect);
	    		}
	    		for(i=0;i<cqlsAEP.m.nbsSimMax;i++) {
					var line=new createjs.Shape();
	    			cqlsAEP.actors.line.push(line);
			    	line.visible=false;
			    	cqlsAEP.m.stage.addChild(line);
	    		}
	    		for(i=0;i<cqlsAEP.m.nbsSimMax;i++) {
	    			var pt=new createjs.Shape();
	    			cqlsAEP.actors.pt.push(pt);
			    	pt.visible=false;
			    	pt.x=0;pt.y=0;
			    	cqlsAEP.m.stage.addChild(pt);
	    		}
			}

			@stage=%x{cqlsAEP.m.stage}
			@plotExp,@plotHist=plotExp,plotHist
			@graphExp,@graphHist=@plotExp.graph,@plotHist.graph
			@graphHist.syncTo(@graphExp)

			## exp needs to be set, @kind describes the kind of experiment
			@exp=[Curve.new,Curve.new]
			setDistrib;setDistrib("chi2",[10],1)
			@plotExp.addChild(@exp[0]);@plotExp.addChild(@exp[1])
			@exp[1].style[:fill]=%x{createjs.Graphics.getRGB(200,200,200,.3)};
			@exp[1].style[:thickness]=1
			#%x{console.log(#{@exp})}

			## hist
			@hist=[Hist.new,Hist.new]
			@plotHist.addChild(@hist[0]);@plotHist.addChild(@hist[1])
			@hist[0].attachCurve(@exp[0]);@hist[1].attachCurve(@exp[1])

			@exp[0].attachSummary;@exp[1].attachSummary
			@hist[0].attachSummary;@hist[1].attachSummary
			@ratioExpAxis=[1-@graphExp.marg[:b]/@graphExp.dim[:h],0.2]
			@yExpAxis=[@ratioExpAxis[0]*@graphExp.dim[:h],@ratioExpAxis[1]*@graphExp.dim[:h]]
			@exp[0].attachExpAxis(@ratioExpAxis[0])
			@exp[1].attachExpAxis(@ratioExpAxis[1])


			@n01=Distribution.new("normal",[0,1]);
			setAlpha(0.05)
			setStatMode(:none)
			isModeHidden? #to initialize @modeHidden

			setTransf
			reset

			# other init
			@x,@y=[],[]
			@aep=[] #containing {cpt: ,step: ,nbTot: ,xRect: ,yRect: }
			@w,@h,@wX,@hY=[],[],[],[]
			@style={fp:"#FFF",sp:"#000000",fl:"#FFF",sl:"#000000",fr:"rgba(100,100,255,0.8)",sr:"#000000"}

			setMLevel(3,:set)
			setN(1) #sample size

		end

		def reset(curs=[0])
			curs << 1 if @transf

			@graphExp.active=@transf ? ["curve0","curve1"] : ["curve0"]
			#####@graphHist.active=@transf ? ["curve0","curve1"] : ["curve0"]
			#p ["ici",@graphExp.active,@graphHist.active]

			@graphExp.update

			@plotExp.update

			#####@graphHist.update
			curs.each do |cur|
				@hist[cur].reset
				@exp[cur].draw
				@hist[cur].drawCurve
				@hist[cur].draw
			end
			@plotHist.update
			setCurHist(@transf ? 1 : 0)
			setTCL;updateTCL(false);
			# hideChildren
			# showChildren
			updateVisible
	    end

	    #########  Distribution

	    def setTCL
	    	unless @checkTCL
		    	@checkTCL=Curve.new
		    	@checkTCL.style={close: true,stroke:"#000",fill:"rgba(100,200,255,0.5)",thickness: 5}
		    	@plotHist.addChild(@checkTCL)
		    end
		    if @transf
		    	case @transf[:name]
		    	when "mean"
	    			@checkTCL.setDistrib("normal",[@exp[0].distrib.mean,%x{Math.sqrt(#{@exp[0].distrib.variance/@n})}])
	    		when "stdMean"
	    			@checkTCL.setDistrib("normal",[0,1])
	    		when "sum"
	    			@checkTCL.setDistrib("normal",[@exp[0].distrib.mean*@n,%x{Math.sqrt(#{@exp[0].distrib.variance*@n})}])
	    		end
	    	end
	    	# p [:setTCL,[@exp[0].distrib.mean,@exp[0].distrib.variance,@exp[0].distrib.variance/@n,%x{Math.sqrt(#{@exp[0].distrib.variance/@n})}],@n]
	    	# p [:ChecTCL,@checkTCL.distrib.mean,@checkTCL.distrib.variance,@checkTCL.distrib.pdf([100])]
	    	# p [:ChecTCL2,@exp[1].distrib.mean,@exp[1].distrib.variance,@exp[1].distrib.pdf([100])]
	    end

	    def updateTCL(state=true)
	    	state=false unless @transf and (["mean","sum","stdMean"].include? @transf[:name])
	    	if state
	    		setTCL
	    		@checkTCL.draw #since update just above does not work (my mistake, I guess)
	    	end
	    	%x{#{@checkTCL}.shape.visible=#{state}}
	    end

		def setDistrib(name="normal",params=nil,cur=0)
			to_set=true
			case name
			when "discreteUniform"
				params=[1,6,1] unless params # dice
			when "bernoulli"
				params=[0.15] unless params
			when "binomial"
				params=[5,0.15] unless params
			when "birthday"
				params=[365,50] unless params
			when "uniform"
				params=[0,1] unless params
			when "stdNormal"
				name="normal"
				params=[0,1] unless params
			when "normal"
				params=[2,0.5] unless params
			when "t"
				params=[10] unless params
			when "chi2"
				params=[10] unless params
			when "exp"
				params=[1] unless params
			when "cauchy"
				params=[0,1] unless params
			when "saljus"
				# fixed mean=100 (90+10) and sd=10 (var=100)
				setDistribAs(Distribution.new("exp",[1],{name: :locationScale, args: [90,10]}))
				# p [:transf_in_saljus, @transf,@curIndHist]
				to_set=false
			end
			@exp[cur].setDistrib(name,params) if to_set
			setTransf(@transf[:name]) if @transf and cur==0
		end

		def setDistribAs(dist,cur=0)
			@exp[cur].setDistribAs(dist)
		end

		def setTransfDistrib(dist,transf,cur=0)
			@exp[cur].setDistribAsTransf(transf,dist)
		end

		def addXY(n=1,cur=0)
			###p [:addXY,@exp[cur].xy]
			xy=@exp[cur].xy(n)
			###p [:xy,xy]
			@x[cur],@y[cur]=xy[:x],xy[:y]
		end

		def setN(n)
			@n=n
			setTransf(@transf[:name]) if @transf #reload transf to set distribution
			setNbSim
		end

		def setStatMode(transf)
			@statMode=(transf==:meanIC ? :ic : :none)
			isModeHidden?
			# p [:setStatMode,transf,@statMode,@modeHidden]
		end

		def setAlpha(alpha)
			@alpha=alpha
		end

		def setMLevel(val=3,mode=:inc)
			return @mLevel if mode==:inc and val==0
			@mLevels=[1,3,5,10,30,100,1000,3000] unless @mLevels
			@mLevel = (mode==:inc ? @mLevel : 0) + val
			@mLevel=0 if @mLevel < 0
			@mLevel=@mLevels.length-1 if @mLevel > @mLevels.length-1
			setNbSim
		end

		def setNbSim
			#p [@n,@mLevel,@mLevels,@n*@mLevels[@mLevel],%x{cqlsAEP.m.nbSimMax}]
			@nbSim=[@n*@mLevels[@mLevel],%x{cqlsAEP.m.nbSimMax}].min
		end

		######## Transformation (maybe to put outside)
		def setTransf(transf=nil) # @transf=nil => no transformation
			@transf=transf
			@transf=nil if @transf==:none
			# Be careful: below, transf and not @transf
			if @transf
				if @n==1 #default if transf fixed before n
					@nold=10 unless @nold
					@n=@nold
					setNbSim
				end
				initTransfList unless @transfList
				@transf=@transfList[transf]
				@transf[:name]=transf
				@transf[:origDist]=dist0=@exp[0].distrib
				#p ["setTransf",dist0.mean,dist0.variance]
				case @transf[:name]
				when "sum"
					@transf[:args]=[@n]
					if @exp[0].distrib.name=="bernoulli"
						@transf[:dist]=:exact
					 	setDistrib("binomial",[@n,dist0.mean],1)
					elsif @exp[0].distrib.name=="normal"
					 	@transf[:dist]=:exact
					 	setDistrib(name="normal",params=[@n*dist0.mean,%x{Math.sqrt(#{dist0.variance*@n})}],1)
					elsif dist0.type==:disc
						@transf[:dist]=:exact
						setTransfDistrib(dist0,@transf,1)
					elsif @n>=30
						@transf[:dist]=:approx
						setDistrib(name="normal",params=[@n*dist0.mean,%x{Math.sqrt(#{dist0.variance*@n})}],1)
					else
						@transf[:dist]=:xylim
						setTransfDistrib(dist0,@transf,1)
					end

				when "mean"
					@transf[:args]=[@n]
					#p @exp[0].distrib.name
					if @exp[0].distrib.name=="bernoulli"
						@transf[:dist]=:exact
					 	setDistribAs(Distribution.new("binomial",[@n,dist0.mean],{name: :locationScale, args: [0,1/@n]}),1)
					elsif @exp[0].distrib.name=="normal"
					 	@transf[:dist]=:exact
					 	setDistrib(name="normal",params=[dist0.mean,%x{Math.sqrt(#{dist0.variance/@n})}],1)
					elsif @exp[0].distrib.name=="cauchy"
						@transf[:dist]=:exact2
					 	setDistrib(name="cauchy",params=[0,1],1)
					elsif dist0.type==:disc
						@transf[:dist]=:exact
						setTransfDistrib(dist0,@transf,1)
					elsif @n>=30
						@transf[:dist]=:approx
						setDistrib(name="normal",params=[dist0.mean,%x{Math.sqrt(#{dist0.variance/@n})}],1)
					else
						@transf[:dist]=:xylim
						setTransfDistrib(dist0,@transf,1)
					end
				when "stdMean"
					@transf[:args]=[dist0.mean]
					if @exp[0].distrib.name=="normal"
					 	@transf[:dist]=:exact
					 	setDistrib(name="t",params=[@n-1],1)
					elsif @n>=30
						@transf[:dist]=:approx
						setDistrib(name="normal",params=[0,1],1)
					else
						@transf[:dist]=:xylim
						setDistrib(name="normal",params=[0,1],1)
					end
				when "sumOfSq"
					@transf[:args]=[@n]
					@transf[:dist]=:exact2
					if @exp[0].distrib.name=="normal"
						setDistrib("chi2",[@n],1)
					else
						setTransfDistrib(dist0,@transf,1)
					end
				when "locationScale"
					@nold,@n=@n,1
					@transf[:dist]=:exact
					@transf[:args]=[-dist0.mean/%x{Math.sqrt(#{dist0.variance})},1/%x{Math.sqrt(#{dist0.variance})}]
					setTransfDistrib(dist0,@transf,1)
				when "square"
					@nold,@n=@n,1
					@transf[:dist]=:exact
					@transf[:args]=[]
					setTransfDistrib(dist0,@transf,1)
				when "center"
					@nold,@n=@n,1
					@transf[:dist]=:exact
					@transf[:transf]=:locationScale
					@transf[:args]=[-dist0.mean,1]
					setTransfDistrib(dist0,{name: @transf[:transf], args: @transf[:args]},1)
				end
			else
				@nold=@n
				@n=1 #used nbsSim
			end
		end

		def animMode # 3 modes
			%x{cqlsAEP.i.anim=cqlsAEP.f.getValue("animMode")}
			%x{cqlsAEP.i.prior=cqlsAEP.f.getValue("priorMode")}
			# %x{console.log(cqlsAEP.i.anim + ":"+ cqlsAEP.i.prior)}
			if %x{cqlsAEP.i.anim}
				if %x{cqlsAEP.i.prior}
					"prior"
				else
					"normal"
				end
			else
				"fast"
			end
		end

		def transfMode
			unless @transf
				:none
			else
				@transf[:mode]
			end
		end

		def initTransfList #we can have only one interface to modify all the values
			@transfList={
				sum: {
					args: [],
					mode: :sample
					},
				mean: {
					args: [],
					mode: :sample
				},
				stdMean: {
					args: [],
					mode: :sample
				},
				sumOfSq: {
					args: [],
					mode: :sample
				},
				locationScale: {
					args: [],
					mode: :all
				},
				square: {
					args: [],
					mode: :all
				},
				addition: {
					args: [],
					mode: :all
				},
				center: {
					args: [],
					mode: :all
				}
			}

		end

		def applyTransfByValue(v)
			method((@transf[:transf]||@transf[:name])+"_transf").call(v,*@transf[:args])
		end

		def applyTransfByIndex(inds,v)
			method((@transf[:transf]||@transf[:name])+"_transf_by_index").call(inds,v,*@transf[:args])
		end

		def sum_transf(v)
			v.inject(0){|e,v2| e+=v2}
		end

		def sum_transf_by_index(inds,v)
			inds.inject(0){|e,i| e+=v[i]}
		end

		def mean_transf(v)
			(v.inject(0){|e,v2| e+=v2})/@n
		end

		def mean_transf_by_index(inds,v)
			(inds.inject(0){|e,i| e+=v[i]})/@n
		end

		def stdMean_transf(v,mu)
			m=(v.inject(0){|e,v2| e+=v2})/@n
			m2=(v.inject(0){|e,v2| e+=v2**2})/@n
			(m-mu)/%x{Math.sqrt((#{m2-m**2})/#{@n-1})}
		end

		def stdMean_transf_by_index(inds,v,mu)
			m=(inds.inject(0){|e,i| e+=v[i]})/@n
			m2=inds.inject(0){|e,i| e+=v[i]**2}/@n
			(m-mu)/%x{Math.sqrt((#{m2-m**2})/#{@n-1})}
		end

		def sumOfSq_transf(v)
			m,s=@transf[:origDist].mean,@transf[:origDist].stdDev
			v.inject(0){|e,v2| e+=((v2-m)/s)**2}
		end

		def sumOfSq_transf_by_index(inds,v)
			m,s=@transf[:origDist].mean,@transf[:origDist].stdDev
			inds.inject(0){|e,i| e+=((v[i]-m)/s)**2}
		end

		def seMean_transf(v)
			m=(v.inject(0){|e,v2| e+=v2})/@n
			m2=(v.inject(0){|e,v2| e+=v2**2})/@n
			#p [:seMean,inds,v,m,m2,%x{Math.sqrt((#{m2-m**2})/#{@n-1})}]
			%x{Math.sqrt((#{m2-m**2})/#{@n-1})}
		end

		def seMean_transf_by_index(inds,v)
			m=(inds.inject(0){|e,i| e+=v[i]})/@n
			m2=(inds.inject(0){|e,i| e+=v[i]**2})/@n
			#p [:seMean,inds,v,m,m2,%x{Math.sqrt((#{m2-m**2})/#{@n-1})}]
			%x{Math.sqrt((#{m2-m**2})/#{@n-1})}
		end

		## vector mode since transfMode==:all
		def locationScale_transf(v)
			v.map{|e| @transf[:args][0]+e*@transf[:args][1]}
		end

		def locationScale_transf_by_index(inds,v)
			 inds.map{|i| @transf[:args][0]+v[i]*@transf[:args][1]}
		end

		def square_transf(v)
			 v.map{|e| e**2}
		end

		def square_transf_by_index(inds,v)
			 inds.map{|i| v[i]**2}
		end


		######## Histo AEP
		def setCurHist(ind=0)
			@curIndHist=ind
			@histCur=@hist[@curIndHist]
			@expCur=@exp[@curIndHist]
		end

		def drawHist #used only when anim paused
			#updateHistAEP
			#TODO!!
		end

		###### Animation tricks

		def allowLevelChange(state) #state=true or false
			%x{cqlsAEP.i.allowLevelChange=#{state}} #required when starting experiment
			@histCur.acceptLevelNext if state
		end

		####### Transition used in a play

		def transitionInitTransf(o=0,t=1)
			if transfMode==:sample
				@ind=[];(0...(@x[o].length/@n)).each {|i| @ind << []} #carefull with [[]]*(@x[o].length/@n)
				(0...@x[o].length).each {|i| @ind[(i/@n).floor] << i}
				@x[t]=[0]*(@x[o].length/@n)
				@icSide,@icGood,@cptIC,q,mu=[0]*(@x[t].length),[0]*(@x[t].length),0,@n01.quantile(1-@alpha/2),@exp[t].distrib.mean if @statMode==:ic
				@col=[]
				@ind.each_with_index do |s,i|
					@x[t][i]=applyTransfByIndex(s,@x[o])
					@col[i]=[%x{Math.random()*256}.floor,%x{Math.random()*256}.floor,%x{Math.random()*256}.floor,0.8]
					if @statMode==:ic #necessarily a mean for now!
						#
						p [:x,@x[o],q]
						@icSide[i]=q*seMean_transf_by_index(s,@x[o])
						@icGood[i]=1 if (@x[t][i]-@icSide[i] <= mu) and (mu <= @x[t][i]+@icSide[i])
						@cptIC += @icGood[i]
					end
				end
				#p [:cptIC,@cptIC,@x[t],@icSide]
				@y[t]=@exp[t].y(@x[t])
			elsif transfMode==:all
				@ind=(0...@x[o].length).map{|i| [i]}
				@x[t]=applyTransfByValue(@x[o])
				@col=[[0,0,0,1]]*(@x[t].length)
				@y[t]=@exp[t].y(@x[t])
				opdf=@exp[o].distrib.pdf(@x[o]).map{|e| e/@exp[o].distrib.step}
				tpdf=@exp[t].distrib.pdf(@x[t]).map{|e| e/@exp[t].distrib.step}
				@y[t]=(0...@x[o].length).map{|i| (@y[o][i])/opdf[i]*tpdf[i]}
				# p [:yo,@y[o]]
				# p [:opdf,opdf]
				# p [:tpdf,tpdf]
				# p [:yt,@y[t]]
			end
		end


		def transitionInitHist(cur,mode=:normal)
			allowLevelChange(false)
			@hist[cur].updateHistAEP(@x[cur], mode)
			@aep[cur]=@hist[cur].aep #to simplify the call
			#p @aep[cur]
			@w[cur],@h[cur]=@aep[cur][:step],1/@aep[cur][:nbTot].to_f/@aep[cur][:step]
			@wX[cur],@hY[cur]=@graphHist.to_X(@w[cur])-@graphHist.to_X(0),@graphHist.to_Y(0)-@graphHist.to_Y(@h[cur])
			#@hNew,@hYNew=@h[cur],@hY[cur] if mode==:new #used inside transitionHistRectsAndPtsHidden
		end

		def transitionInitPts(cur)
			###p [:transitionInitPts,@x[cur]]
			(0...@x[cur].length).each do |i|
				%x{
					//draw points
					cqlsAEP.actors.pt[i].graphics.c().s(#{@style[:sp]}).f(#{@style[:fp]}).drawCircle(0,0,cqlsAEP.i.ptSize);
					//tweens for points
					cqlsAEP.tweens.pt[i]=createjs.Tween.get(cqlsAEP.actors.pt[i],{override:true});
				}
				if @hist[cur].type==:disc
					%x{
						//draw lines
						cqlsAEP.actors.line[i].graphics.c().s(#{@style[:sl]}).f(#{@style[:fl]})
						.drawRect(0,0,#{@wX[cur]},0);
						cqlsAEP.actors.line[i].regX=#{@wX[cur]}/2.0;
						//tweens for lines
						cqlsAEP.tweens.line[i]=createjs.Tween.get(cqlsAEP.actors.line[i],{override:true});
					}
				end
			end
		end

		def transitionInitPtsTransf(cur)
			return unless @transf[:mode]==:sample
			if cur==0
				@ind.each_with_index do |s,i2|
					col="rgba(#{@col[i2][0]},#{@col[i2][1]},#{@col[i2][2]},#{@col[i2][3]})"
					s.each do |i|
						%x{
							cqlsAEP.actors.pt[i].graphics.c().s(#{@style[:sp]}).f(#{col}).drawCircle(0,0,cqlsAEP.i.ptSize);
							cqlsAEP.actors.line[i].graphics.c().s(#{col}).f(#{@style[:fl]}).drawRect(0,0,#{@wX[cur]},2);
						}
					end
				end
			else
				(0...@x[cur].length).each_with_index do |i|
					col="rgba(#{@col[i][0]},#{@col[i][1]},#{@col[i][2]},#{@col[i][3]})"
 					%x{
						cqlsAEP.actors.pt[i].graphics.c().s(#{@style[:sp]}).f(#{col}).drawCircle(0,0,cqlsAEP.i.ptSize);
					}
					if @hist[cur].type==:disc
						%x{
							//cqlsAEP.actors.line[i].graphics.c().s(#{col}).f(#{@style[:fl]}).drawRect(0,0,#{@wX[cur]},2);
							cqlsAEP.tweens.line[i].call(function(tween) {
					 			tween._target.graphics.c().s(#{col}).f(#{@style[:fl]}).drawRect(0,0,#{@wX[cur]},2);
					 		})
					}
					end
				end
			end

		end

		def transitionInitTime
			@time=0
		end

		def transitionInitExpRects(cur)
			if @modeHidden
				scale=(@graphExp.dim[:h]*0.2)/(@x[cur].length)
				scale=1 if scale > 1
			end
			(0...@x[cur].length).each do |i|
				y=@yExpAxis[0] - (@hist[cur].type==:disc ? (i*scale) : 0)
				%x{
					//draw rect first
					cqlsAEP.actors.rect[i].x=#{@graphExp.to_X(@aep[cur][:xRect][i])};cqlsAEP.actors.rect[i].y=#{y};
					cqlsAEP.actors.rect[i].regY=#{@hY[cur]/2};
					cqlsAEP.actors.rect[i].graphics.c().f(#{@style[:fr]}).s(#{@style[:sr]}).drawRect(0,0,#{@wX[cur]},#{@hY[cur]});
					cqlsAEP.tweens.rect[i]=createjs.Tween.get(cqlsAEP.actors.rect[i],{override:true});
				}
			end

		end

		def transitionInitRects(cur)
			(0...@x[cur].length).each do |i|
				%x{
					//draw rect first
					cqlsAEP.actors.rect[i].x=#{@graphHist.to_X(@aep[cur][:xRect][i])};cqlsAEP.actors.rect[i].y=#{@plotHist.dim[:y]};
					cqlsAEP.actors.rect[i].regY=#{@hY[cur]/2};
					cqlsAEP.actors.rect[i].graphics.c().f(#{@style[:fr]}).s(#{@style[:sr]}).drawRect(0,0,#{@wX[cur]},#{@hY[cur]});
					cqlsAEP.tweens.rect[i]=createjs.Tween.get(cqlsAEP.actors.rect[i],{override:true});
				}
			end
		end

		## Timing[0,500,1000,1500]
		def transitionDrawPts(cur,wait=1000*%x{cqlsAEP.i.scaleTime})
			#p [:drawPts,cur,@hist[cur].type]
			if @modeHidden
				scale=(@graphExp.dim[:h]*0.2)/(@x[cur].length)
				scale=1 if scale > 1
				@remember={:lag => wait/@x[cur].length} if cur==0
			end

			(0...@x[cur].length).each do |i|
				y=@modeHidden ? @yExpAxis[0] : @graphExp.to_Y(@y[cur][i])
				y -= i*scale if @modeHidden and @hist[cur].type==:disc
				wait2=wait
				wait2 -= i*@remember[:lag] if @modeHidden and cur==0 and @transf #only for transf
				%x{
					cqlsAEP.tweens.pt[i].to({x:#{@graphExp.to_X(@x[cur][i])},y:#{y}})
					.set({visible:true})
					.wait(#{wait2})
				}
				if @hist[cur].type==:disc and !@modeHidden
					#p [:drawPt,i,@graphExp.to_X(@x[cur][i]),@graphExp.to_Y(@y[cur][i])]
					%x{
						cqlsAEP.tweens.line[i].to({x:#{@graphExp.to_X(@x[cur][i])},y:#{@graphExp.to_Y(@y[cur][i])}})
						.set({visible:true})
						.wait(#{wait});
					}
				end
			end
			@time+=wait
		end

		def transitionPtsTransf(t=1,merge=1500*%x{cqlsAEP.i.scaleTime},wait=500*%x{cqlsAEP.i.scaleTime})
			if @modeHidden
				scale=(@graphExp.dim[:h]*0.2)/(@ind.length)
				scale=1 if scale > 1
			end

			@ind.each_with_index do |s,i2|
				col="rgba(#{@col[i2][0]},#{@col[i2][1]},#{@col[i2][2]},#{@col[i2][3]})"
				y=@modeHidden ? @yExpAxis[1] : @graphExp.to_Y(@y[t][i2])
				y -= i2*scale if @modeHidden and @hist[t].type==:disc
				s.each do |i|
					wait2=wait
					wait2 -= (@n-i)*@remember[:lag] if @modeHidden
					%x{
						cqlsAEP.tweens.pt[i].to({x:#{@graphExp.to_X(@x[t][i2])},y:#{y}},#{merge})
						if(#{@modeHidden}) cqlsAEP.tweens.pt[i].to({y:#{@graphExp.to_Y(0)}},#{merge})
						cqlsAEP.tweens.pt[i].wait(#{wait2}).set({visible:false})
						if(#{@transf and @hist[0].type==:disc and @hist[1].type==:disc and !@modeHidden}) {
					 		cqlsAEP.tweens.line[i].call(function(tween) {
					 			tween._target.regX=#{@wX[t]}/2.0;
					 			tween._target.graphics.c().s(#{col}).f(#{@style[:fl]}).drawRect(0,0,#{@wX[t]},2);
					 		})
					 		.to({x:#{@graphExp.to_X(@x[t][i2])},y:#{@graphExp.to_Y(@y[t][i2])}},#{merge})
							.wait(#{wait}).set({visible:false})
					 		//cqlsAEP.tweens.line[i].wait(#{wait+merge}).set({visible:false});
						}
					}
				end
			end
			@time+=merge+wait
			@time+=merge if @modeHidden
		end

		def transitionFallPts(cur,fall=2000*%x{cqlsAEP.i.scaleTime},wait=1000*%x{cqlsAEP.i.scaleTime})
			%x{cqlsAEP.durations.ptsBeforeFall=#{@time}}
			(0...@x[cur].length).each do |i|
				%x{
					cqlsAEP.tweens.pt[i].to({y:#{@plotHist.dim[:y]}},#{fall},createjs.Ease.bounceOut)
					.wait(#{wait})
				}
				if @hist[cur].type==:disc
					%x{
						cqlsAEP.tweens.line[i].to({y:#{@plotHist.dim[:y]}},#{fall},createjs.Ease.bounceOut)
						.wait(#{wait}).set({visible:false});
					}
				end
			end
			@time+=fall+wait
		end


		def transitionExpPtsAndRects(cur,from=@time,before=2000*%x{cqlsAEP.i.scaleTime},fall=1000*%x{cqlsAEP.i.scaleTime},after=1000*%x{cqlsAEP.i.scaleTime})
			fall+=@x[cur].length
			(0...@x[cur].length).each do |i|
				%x{
					cqlsAEP.tweens.pt[i].wait(#{before}+i)
					.to({y:#{@graphExp.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

					//rect start here so wait "from" ms first
					cqlsAEP.tweens.rect[i].set({visible:false})
					.wait(#{from}).set({visible:true})
					.wait(#{before}+i)
					.to({y:#{@graphExp.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

				}
			end
			@time += before+fall+after
		end

		def transitionDrawRectsHidden(cur,from=@time,after=500*%x{cqlsAEP.i.scaleTime})
			#p [@wX[cur],@hY[cur]]
			(0...@x[cur].length).each do |i|
				%x{

					cqlsAEP.tweens.pt[i].to({y:#{@graphExp.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0})
					.wait(#{after});

					if(i==0) {
						cqlsAEP.tweens.rect[i].call(function(tween) {
							#{@hist[cur].draw(@aep[cur][:nbTot])};
							#{allowLevelChange(true)};
						})
					}
					//redraw rect first
					cqlsAEP.tweens.rect[i].call(function(tween) {
						tween._target.regY=#{@hY[cur]/2};
					 	tween._target.graphics.c().f(#{@style[:fr]}).s(#{@style[:sr]}).drawRect(0,0,#{@wX[cur]},#{@hY[cur]});
					})
					.to({y:#{@graphExp.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0})
					.wait(#{after});

				}
			end
			@time += after
		end

		def transitionHistPtsAndRectsHidden(cur,from=@time,before=1000*%x{cqlsAEP.i.scaleTime},fall=1000*%x{cqlsAEP.i.scaleTime},after=1000*%x{cqlsAEP.i.scaleTime})
			fall+=@x[cur].length
			(0...@x[cur].length).each do |i|
				%x{
					cqlsAEP.tweens.pt[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

					cqlsAEP.tweens.rect[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

				}
			end
			%x{
				//only once
				cqlsAEP.tweens.pt[0].call(function(tween) {
						#{hideAll(cur);@hist[cur].add(@x[cur]);@hist[cur].draw;drawSummary(cur)};
				})
			}
			@time += before+fall+after
		end




		def transitionHistPtsAndRects(cur,from=@time,before=2000*%x{cqlsAEP.i.scaleTime},fall=1000*%x{cqlsAEP.i.scaleTime},after=1000*%x{cqlsAEP.i.scaleTime})
			fall+=@x[cur].length
			(0...@x[cur].length).each do |i|
				%x{
					cqlsAEP.tweens.pt[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

					//rect start here so wait "from" ms first
					cqlsAEP.tweens.rect[i].set({visible:false})
					.wait(#{from}).set({visible:true})
					if(i==0) {
						cqlsAEP.tweens.rect[i].call(function(tween) {
							#{@hist[cur].draw(@aep[cur][:nbTot])};
							#{allowLevelChange(true)};
						})
					}
					cqlsAEP.tweens.rect[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

				}
			end
			%x{
				//only once
				cqlsAEP.tweens.pt[0].call(function(tween) {
						#{hideAll(cur);@hist[cur].add(@x[cur]);@hist[cur].draw;drawSummary(cur)};
				})
			}
			@time += before+fall+after
		end


		def transitionDrawIC(from=@time,wait=1000*%x{cqlsAEP.i.scaleTime},pause=1000*%x{cqlsAEP.i.scaleTime},before=1000*%x{cqlsAEP.i.scaleTime},fall=1000*%x{cqlsAEP.i.scaleTime},after=1000*%x{cqlsAEP.i.scaleTime})
			#p [:transIC,@icSide,@icSide.map{|e| @graphExp.to_X(e)},@icGood]
			@ind.each_with_index do |s,i2|
				col="rgba(#{@col[i2][0]},#{@col[i2][1]},#{@col[i2][2]},#{@col[i2][3]})"
				y=@graphExp.to_Y(@y[1][i2])
				l=@graphExp.to_X(@icSide[i2])-@graphExp.to_X(0)
				%x{cqlsAEP.tweens.pt[i2].wait(#{pause})}
				if @hist[1].type==:disc #
					%x{
						cqlsAEP.tweens.line[i2]
						.call(function(tween) {
				 			tween._target.regX=#{l};
				 			tween._target.regY=1;
				 			tween._target.graphics.c().s(#{col}).f(#{@style[:fl]}).drawRect(0,0,#{2*l},2);
					 	})
						.wait(#{2*pause})

					}
				else #create them
					%x{
						//draw lines
						cqlsAEP.actors.line[i2].graphics.c().s(#{col}).f(#{@style[:fl]})
						.drawRect(0,0,#{2*l},2);
						cqlsAEP.actors.line[i2].regX=#{l};
						cqlsAEP.actors.line[i2].regY=1;
						//tweens for lines
						cqlsAEP.tweens.line[i2]=createjs.Tween.get(cqlsAEP.actors.line[i2],{override:true});
						cqlsAEP.tweens.line[i2].set({visible:false}).wait(#{from}+#{wait})
						.to({x:#{@graphExp.to_X(@x[1][i2])},y:#{y}})
				 		.set({visible:true}).wait(#{pause})
				 	}
				end
				if @icGood[i2]==0
					%x{
						cqlsAEP.tweens.pt[i2].wait(#{pause}).to({scaleX: 2.0,scaleY: 2.0},#{pause}); //.to({scaleX:1.0,scaleY:1.0})
						cqlsAEP.tweens.line[i2].to({scaleY: 3.0},#{pause}).to({scaleY: 1.0});
					}
				else
					%x{
						cqlsAEP.tweens.pt[i2].wait(#{2*pause});
						cqlsAEP.tweens.line[i2].wait(#{pause});
					}
				end
			end
			@time+=3*pause

			## adaptation of transitionHistPtsAndRects
			cur=1
			fall += @x[cur].length
			(0...@x[cur].length).each do |i|
				%x{
					//rect start here so wait "@time" ms first
					cqlsAEP.tweens.rect[i].set({visible:false})
					.wait(#{@time})
					.to({x:#{@graphExp.to_X(@aep[cur][:xRect][i])},y:#{@graphExp.to_Y(@y[1][i])}})
					.set({visible:true});
					if(i==0) {
						cqlsAEP.tweens.rect[i].call(function(tween) {
							#{@hist[cur].draw(@aep[cur][:nbTot])};
							#{allowLevelChange(true)};
						});
					}

					cqlsAEP.tweens.pt[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});
					cqlsAEP.tweens.line[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});

					cqlsAEP.tweens.rect[i].wait(#{before}+i)
					.to({y:#{@graphHist.to_Y(@aep[cur][:yRect][i])}+#{@hY[cur]}/2.0},#{fall}-i)
					.wait(#{after});
					if(#{@icGood[i]==0}) {
						cqlsAEP.tweens.pt[i].to({scaleX:1.0,scaleY:1.0});
						cqlsAEP.tweens.line[i].to({scaleY:1.0});
					}

				}
			end
			%x{
				//only once
				cqlsAEP.tweens.pt[0].call(function(tween) {
						#{hideAll(cur);@hist[cur].add(@x[cur]);@hist[cur].incCptIC(@cptIC);@hist[cur].draw;drawSummary(cur)};
				});
			}
			@time += before+fall+after
		end

		def hideAll(cur)
			if @x[cur]
				# (0...@x[cur].length).each{|i|
				# 	%x{
				# 		cqlsAEP.actors.pt[i].visible=false;
				# 		cqlsAEP.actors.line[i].visible=false;
				# 		cqlsAEP.actors.rect[i].visible=false;
				# 	}
				# }
				%x{
					for(i=0;i<cqlsAEP.m.nbsSimMax;i++) {
						cqlsAEP.actors.pt[i].visible=false;
						cqlsAEP.actors.line[i].visible=false;
						cqlsAEP.actors.rect[i].visible=false;
					}
				}
			end

		end

		# def show
		# 	isTransf=transfMode != :none
		# 	isSample=transfMode==:sample
		# 	%x{
		# 		#{@exp[0]}.shape.visible=cqlsAEP.enyo.app.$.checkExp0Curve.getValue();
		# 		#{@exp[1]}.shape.visible=#{isTransf} & cqlsAEP.enyo.app.$.checkExp1Curve.getValue();
		# 		#{@hist[0]}.shape.visible=#{!isTransf};
		# 		#{@hist[1]}.shape.visible=#{isTransf};
		# 		#{@hist[0]}.curveShape.visible=#{!isTransf} & cqlsAEP.enyo.app.$.checkHistCurve.getValue();
		# 		#{@hist[1]}.curveShape.visible=#{isTransf} & cqlsAEP.enyo.app.$.checkHistCurve.getValue();
		# 		#{@hist[0]}.summaryShapes[0].visible=false;
		# 		#{@hist[1]}.summaryShapes[0].visible=false;
		# 		#{@checkTCL}.shape.visible=#{isSample} & cqlsAEP.enyo.app.$.checkTCL.getValue();
		# 	}
		# 	showExpAxis
		# 	showSummary
		# end

		# def showExpAxis
		# 	isTransf = transfMode != :none
		# 	%x{#{@exp[0]}.expAxisShape.visible= !cqlsAEP.enyo.app.$.checkExp0Curve.getValue()}
		# 	%x{#{@exp[1]}.expAxisShape.visible= false} #!cqlsAEP.enyo.app.$.checkExp0Curve.getValue() & #{isTransf}}

		# end

		def drawSummary(cur=@curIndHist)
			## AEP only since the others do not change
			@hist[cur].drawMean
			@hist[cur].drawSD
			state=%x{cqlsAEP.f.getValue("checkSummary")}
			# %x{#{@hist[cur]}.summaryShapes[0].visible=#{state}}
			# %x{#{@hist[cur]}.summaryShapes[1].visible=#{state}}
		end

		# def showSummary
		# 	state=%x{cqlsAEP.enyo.app.$.checkSummary.getValue()}
		# 	isTransf = transfMode != :none
		# 	%x{#{@exp[0]}.summaryShapes[0].visible=#{state} & cqlsAEP.enyo.app.$.checkExp0Curve.getValue()}
		# 	%x{#{@exp[0]}.summaryShapes[1].visible=#{state} & cqlsAEP.enyo.app.$.checkExp1Curve.getValue()}
		# 	%x{#{@exp[1]}.summaryShapes[0].visible=#{state && isTransf} & cqlsAEP.enyo.app.$.checkExp0Curve.getValue()}
		# 	%x{#{@exp[1]}.summaryShapes[1].visible=#{state && isTransf} & cqlsAEP.enyo.app.$.checkExp1Curve.getValue()}
		# 	%x{#{@histCur}.summaryShapes[0].visible=#{state}}
		# 	%x{#{@histCur}.summaryShapes[1].visible=#{state}}
		# end

		# def initEnyoMode
		# 	unless @enyoMode
		# 		@enyoMode={
		# 			"exp0Curve" => "checkExp0Curve",
		# 			"exp1Curve" => "checkExp1Curve",
		# 			"summary" => "checkSummary",
		# 			"tclCurve" => "checkTCL",
		# 			"histCurve" => "checkHistCurve"
		# 		}
		# 	end
		# end

		def updateVisible #from enyo interface
			isTransf = transfMode != :none
			isSample = transfMode == :sample
			%x{
				#{@exp[0]}.shape.visible=cqlsAEP.f.getValue("checkExp0Curve");
				#{@exp[1]}.shape.visible=#{isTransf} & cqlsAEP.f.getValue("checkExp1Curve");
				#{@hist[0]}.shape.visible=#{!isTransf};
				#{@hist[1]}.shape.visible=#{isTransf};
				#{@hist[0]}.curveShape.visible=#{!isTransf} & cqlsAEP.f.getValue("checkHistCurve");
				#{@hist[1]}.curveShape.visible=#{isTransf} & cqlsAEP.f.getValue("checkHistCurve");
				#{@hist[0]}.summaryShapes[0].visible=false;
				#{@hist[1]}.summaryShapes[0].visible=false;
				#{@hist[0]}.summaryShapes[1].visible=false;
				#{@hist[1]}.summaryShapes[1].visible=false;
				#{@checkTCL}.shape.visible=#{isSample} & cqlsAEP.f.getValue("checkTCL");
			}

			## Axis
			%x{#{@exp[0]}.expAxisShape.visible= !cqlsAEP.f.getValue("checkExp0Curve")}
			%x{#{@exp[1]}.expAxisShape.visible= false} #!cqlsAEP.f.getValue("checkExp0Curve") & #{isTransf}}

			## Summary
			state=%x{cqlsAEP.f.getValue("checkSummary")}
			%x{#{@exp[0]}.summaryShapes[0].visible=cqlsAEP.f.getValue("checkExp0Mean")}
			%x{#{@exp[0]}.summaryShapes[1].visible=cqlsAEP.f.getValue("checkExp0SD")}
			%x{#{@exp[1]}.summaryShapes[0].visible=#{isTransf} & cqlsAEP.f.getValue("checkExp1Mean")}
			%x{#{@exp[1]}.summaryShapes[1].visible=#{isTransf} & cqlsAEP.f.getValue("checkExp1SD")}
			%x{#{@histCur}.summaryShapes[0].visible=cqlsAEP.f.getValue("checkHistMean")}
			%x{#{@histCur}.summaryShapes[1].visible=cqlsAEP.f.getValue("checkHistSD")}
			## TCL
			updateTCL(%x{cqlsAEP.f.getValue("checkTCL")})
			# update stage since possible change of visibility
			%x{cqlsAEP.m.stage.update()}
		end


		####### Reset play

		# def resetShort
		# 	setExperiment

		# end

		##### play

		def playShort(cur=@curIndHist,duration=500)
			hideAll(cur) # todo resetAll instead
			animMode
			@time=0
			if @transf and (@transf[:dist]!=:exact or @statMode==:ic)
				x=[]
				q,mu=@n01.quantile(1-@alpha/2),@exp[0].distrib.mean if @statMode==:ic
				(0...(10 ** %x{cqlsAEP.i.count})).each do |i|
					xx=@exp[0].sample(@n)
					x[i]=applyTransfByValue(xx)
					if @statMode==:ic
						icSide=q*seMean_transf(xx)
						@hist[cur].cptICTot += 1  if (x[i]-icSide <= mu) and (mu <= x[i]+icSide)
					end
				end
				# p [:x,x]
				# p [:bounds,@hist[cur].bounds]
				# p [:bounds2,@exp[cur].bounds]
				@hist[cur].add(x)
			else
				@hist[cur].add(@exp[cur].sample(10 ** %x{cqlsAEP.i.count}))
			end
			@hist[cur].draw
			drawSummary(cur)
			%x{cqlsAEP.m.stage.update()}
			playNextAfter(duration)
		end

		def playLongDensityBasic(duration=1000)
			addXY(@nbSim)
			transitionInitHist(0)
			transitionInitPts(0)
			transitionInitRects(0)
			transitionInitTime
			transitionDrawPts(0)
			transitionFallPts(0)
			transitionHistPtsAndRects(0)
			playNextAfter(@time+duration)
		end

		def playLongDensityBasicHidden(duration=1000)
			###p [:playLongDensityBasicHidden,@nbSim]
			addXY(@nbSim)
			transitionInitHist(0,:new)
			transitionInitPts(0)
			transitionInitExpRects(0)
			transitionInitTime
			transitionDrawPts(0)
			transitionExpPtsAndRects(0)
			transitionInitHist(0,:reduced)
			transitionDrawRectsHidden(0)
			transitionInitHist(0)
			transitionHistPtsAndRectsHidden(0)
			playNextAfter(@time+duration)
		end

		def playLongDensityWithTransf(duration=1000)
			addXY(@nbSim)
			transitionInitTransf(0,1)
			transitionInitHist(0) #to get info on step in particular
			transitionInitHist(1)
			transitionInitPts(0)
			transitionInitPtsTransf(0)
			transitionInitRects(1)
			transitionInitTime
			transitionDrawPts(0)
			transitionPtsTransf(1)
			transitionInitPtsTransf(1)
			transitionDrawPts(1)
			transitionFallPts(1)
			transitionHistPtsAndRects(1)
			playNextAfter(@time+duration)
		end

		def playLongDensityWithTransfHidden(duration=1000)
			addXY(@nbSim)
			transitionInitTransf(0,1)
			transitionInitHist(0) #to get info on step in particular
			transitionInitHist(1,:new)
			transitionInitPts(0)
			transitionInitPtsTransf(0)
			transitionInitExpRects(1)
			transitionInitTime
			transitionDrawPts(0)
			transitionPtsTransf(1)
			transitionInitPtsTransf(1)
			transitionDrawPts(1)
			transitionExpPtsAndRects(1)
			transitionInitHist(1,:reduced)
			transitionDrawRectsHidden(1)
			transitionInitHist(1)
			transitionHistPtsAndRectsHidden(1)
			playNextAfter(@time+duration)
		end

		def playLongDensityForIC(duration=1000)
			addXY(@nbSim)
			transitionInitTransf(0,1)
			transitionInitHist(0) #to get info on step in particular
			transitionInitHist(1)
			transitionInitPts(0)
			transitionInitPtsTransf(0)
			transitionInitRects(1)
			transitionInitTime
			transitionDrawPts(0)
			transitionPtsTransf(1)
			transitionInitPtsTransf(1)
			transitionDrawPts(1)
			transitionDrawIC
			# transitionFallPts(1)
			# transitionHistPtsAndRects(1)
			playNextAfter(@time+duration)
		end

		def isModeHidden?
			animMode
			@modeHidden=%x{!cqlsAEP.i.prior}
			@modeHidden=false if @statMode==:ic
			#p [:modeHidden,@modeHidden]
			return @modeHidden
		end

		def playLongDensity(duration=1000)
			###p [@transf,isModeHidden?]

			if @transf

				if isModeHidden?
					playLongDensityWithTransfHidden(duration)
				elsif @statMode==:ic
					playLongDensityForIC(duration)
				else
					playLongDensityWithTransf(duration)
				end
			else
				if isModeHidden?
					playLongDensityBasicHidden(duration)
				else
					playLongDensityBasic(duration)
				end
			end
		end


		def playNextAfter(duration)
			%x{
				createjs.Tween.get(cqlsAEP.m.stage,{override:true}).wait(#{duration}).call(
					function(tween) {
						if(cqlsAEP.i.loop) cqlsAEP.f.updateDemo();
					}
				);
			}
		end



	end

	# class Timing

	# 	attr_accessor :d, :t

	# 	def Timing.[](*ary)
	# 		Timing.new(ary)
	# 	end

	# 	def initialize(durations=[])
	# 		@d=durations
	# 		@t=[0]*(@d.length)
	# 		initTime
	# 	end

	# 	def initTime
	# 		s=0
	# 		@d.each_with_index {|d,i|
	# 			s+=@d[i]
	# 			@t[i]=s
	# 		}
	# 	end

	# 	def start
	# 		@t[0]
	# 	end

	# 	def stop
	# 		@t[-1]
	# 	end

	# 	def length
	# 		@t[-1]-@t[0]
	# 	end

	# end

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
						qbounds: [%x{cqlsAEP.m.qmin},%x{cqlsAEP.m.qmax}]
					},
					t: {
						type: :cont,
						dist:["StudentDistribution"],
						qbounds: [%x{cqlsAEP.m.qmin},%x{cqlsAEP.m.qmax}]
					},
					chi2: {
						type: :cont,
						dist: ["ChiSquareDistribution"],
						qbounds: [0,%x{cqlsAEP.m.qmax}]
					},
					exp: {
						type: :cont,
						dist: ["ExponentialDistribution"],
						qbounds: [0,%x{cqlsAEP.m.qmax}]
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
					###p [:boundsDistrib,step,bounds,pdf(bounds),pdf(bounds).inject(0) {|e,e2| e += e2}]

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
			%x{z=[];for(i=0;i<#{n};i++) z[i]=#{@distrib}.simulate()}
			%x{z}
		end

		def pdf(x)
			%x{#{x}.map(function(e) {return #{@distrib}.density(e);})}
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
					v=CqlsAEP.quantize(v1+v2)
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

	def CqlsAEP.quantize(x,prec=PREC4DISC)
		%x{parseFloat(#{x}.toFixed(#{prec}))}
	end

	def CqlsAEP.equal(a,b)
		%x{a.toFixed(#{PREC4DISC})===b.toFixed(#{PREC4DISC})}
	end

	def CqlsAEP.range(low, high, step) # from http://phpjs.org/functions/range/
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

	def CqlsAEP.seq(min, max, length) #from jStat
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
