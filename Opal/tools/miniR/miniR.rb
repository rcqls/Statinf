require 'opal'
require 'opal-parser'

class Distribution

		attr_accessor :list, :name, :params, :distrib

		def initialize(name=nil,params=[])
			unless @@list
				@@list={
					unif: {
						type: :cont,
						dist: ["UniformDistribution"]
					},
					norm: {
						type: :cont,
						dist: ["NormalDistribution"]
					},
					t: {
						type: :cont,
						dist:["StudentDistribution"]
					},
					f: {
						type: :cont,
						dist:["FDistribution"]
					},
					chisq: {
						type: :cont,
						dist: ["ChiSquareDistribution"]
					},
					exp: {
						type: :cont,
						dist: ["ExponentialDistribution"]
					},
					cauchy: {
						type: :cont,
						dist: ["CauchyDistribution"]
					},
					discreteUniform: {
						type: :disc,
						dist: ["DiscreteUniformDistribution"]
					},
					bernoulli: {
						type: :disc,
						dist: ["BernoulliDistribution"]
					},
					binom: {
						type: :disc,
						dist: ["BinomialDistribution"]
					},
					birthday: {
						type: :disc,
						dist: ["BirthdayDistribution"]
					}
				}
			end
			@list=@@list
			if name
			  set(name,params)
			end
		end

		def set(dist,params)
			@name,@params=dist,params
			#p [@name,@list[@name]]
			@type=@list[@name][:type]
			instr="new "+@list[@name]["dist"].join(".")+"("+@params.join(',')+");"
			@distrib=%x{eval(#{instr})}
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
			z=[]
			1.upto(n) {|i| z << %x{#{@distrib}.simulate()}}
			return RVector.new(z)
		end

		def pdf?(x,ary=false)
			unless ary
				return %x{#{@distrib}.density(#{x});}
			else
				return RVector.new(x.to_a.map{|e| pdf?(e)})
			end
		end

		def pdf(x)
			pdf?(x,([Array,RVector].include? x.class))
		end

		def cdf?(x,ary=false)
			unless ary
				%x{#{@distrib}.CDF(#{x})}
			else
				return RVector.new(x.to_a.map{|e| cdf?(e)})
			end
		end

		def cdf(x)
			cdf?(x,([Array,RVector].include? x.class))
		end

		def quantile?(p,ary=false)
			unless ary
				%x{#{@distrib}.quantile(#{p})}
			else
				return RVector.new(p.to_a.map{|e| quantile?(e)})
			end
		end

		def quantile(p)
			quantile?(p,([Array,RVector].include? p.class))
		end

end

class RVector

  def initialize(vals)
    @y=%x{new CompleteData()}
    vals.each do |val|
      %x{#{@y}.setValue(#{val})};
    end
  end

	def to_a
		%x{#{@y}.getValues()}
	end

	def coerce(x)
			return [RVector.new(x.to_a),self]
	end

  def [](i)
    to_a[i-1]
  end

	def _recycle_(x)
		ary1,ary2=to_a,x.to_a
		s1,s2=ary1.size,ary2.size
		s=[s1,s2].max
		[s,(ary1*(s/s1).ceil)[0...s],(ary2*(s/s2).ceil)[0...s]]
	end

	def -@
		RVector.new to_a.map{|e| -e}
	end

	def +@
		self
	end

  def *(x)
		res=nil
    if x.is_a? RVector
			s,a1,a2=_recycle_(x)
      res=(0...s).map{|i| a1[i]*a2[i]}
    else
       res=to_a.each_with_index.map{|e,i| e*x}
    end
		RVector.new res
  end

	def /(x)
		res=nil
    if x.is_a? RVector
			s,a1,a2=_recycle_(x)
      res=(0...s).map{|i| a1[i]/a2[i]}
			# xx=x.to_a
      # res=to_a.each_with_index.map{|e,i| e/xx[i]}
    else
       res=to_a.each_with_index.map{|e,i| e/x}
    end
		RVector.new res
  end

	def +(x)
		res=nil
    if x.is_a? RVector
			s,a1,a2=_recycle_(x)
      res=(0...s).map{|i| a1[i]+a2[i]}
			# xx=x.to_a
      # res=to_a.each_with_index.map{|e,i| e+xx[i]}
    else
       res=to_a.each_with_index.map{|e,i| e+x}
    end
		RVector.new res
  end

	def -(x)
		res=nil
    if x.is_a? RVector
			s,a1,a2=_recycle_(x)
      res=(0...s).map{|i| a1[i]-a2[i]}
			# xx=x.to_a
      # res=to_a.each_with_index.map{|e,i| e-xx[i]}
    else
       res=to_a.each_with_index.map{|e,i| e-x}
    end
		RVector.new res
  end

	def **(x)
		res=nil
    if x.is_a? RVector
			s,a1,a2=_recycle_(x)
      res=(0...s).map{|i| a1[i]**a2[i]}
    else
       res=to_a.each_with_index.map{|e,i| e**x}
    end
		RVector.new res
  end

=begin
	## Aliases obsolete soon!
	alias add +
	alias substract -
	alias multiply *
	alias divide /
=end

	def to_Routput(width=60,round=8)
		tmp=to_a
		l=tmp.map{|e| e.round.to_s.length}.max
		p = (l > round ? 1 : round - l)
		ll = l + p +1
		ncol = (width / ll).to_i 
		nrow = ((size / ncol).floor + (size % ncol > 0 ? 1 : 0)).to_i
		lines = (0...nrow).map do |i|
			index="% #{(ncol*(nrow-1)).to_s.length+2}s" % "[#{(i*ncol)+1}]"
			line = ((i*ncol)..(i*ncol+ncol-1)).map{ |e| tmp[e] ? ("% #{ll}s" % ("%.#{p}f" %  tmp[e]) ) : "" }.join("  ")
			index + " " + line 
		end
		lines.join("\n")
	end

	def to_s
		to_a.to_s
	end

	def min
		to_a.min
	end

	def max
		to_a.max
	end

	def range
		tmp=to_a
		[tmp.min,tmp.max]
	end

	def diff
		tmp=to_a
		(1...size).map{|i| tmp[i]-tmp[i-1]}
	end

	def sum 
		to_a.inject :+
	end

	def prod 
		to_a.inject :*
	end

	def cumsum
		to_a.inject([]) { |x, y| x + [(x.last || 0) + y] }
	end

	def cumprod
		to_a.inject([]) { |x, y| x + [(x.last || 0) * y] }
	end

	def cummin
		to_a.inject([]) { |x, y| x + [[x.last || y,y].min] }
	end

	def cummax
		to_a.inject([]) { |x, y| x + [[x.last || y,y].max] }
	end

	def mean
		%x{#{@y}.mean()}
	end

	def var
	  %x{#{@y}.variance()}
	end

	def sd
	  %x{#{@y}.stdDev()}
	end

	def quantile(p)
	  %x{#{@y}.quantile(#{p})}
	end

	def size
		%x{#{@y}.size()}
	end

	def seMean
		#%x{#{@y}.stdDev()/Math.sqrt(#{@y}.size())}
		sd/Math.sqrt(size)
	end

	def squareFromMean
	  aryMean=mean
		return RVector.new to_a.map{|e| (e-aryMean)**2}
	end

	def seVar
	  return squareFromMean.seMean
	end

	def sqrt
		RVector.new to_a.map{|e| Math.sqrt(e)}
	end

end

class Numeric

=begin
	## Obsolete soon since coerce method of RVector makes the job!
	def add(x)
		if x.is_a? RVector
			x.add self
		else
			self + x
		end
	end

	def multiply(x)
		if x.is_a? RVector
			x.multiply self
		else
			self * x
		end
	end

	def substract(x)
    if x.is_a? RVector
      return RVector.new x.to_a.map{|e| self - e}
    else
       return self - x
    end
	end

	def divide(x)
		if x.is_a? RVector
      return RVector.new x.to_a.map{|e| self / e}
    else
       return self / x
    end
	end
=end

	def to_a
		[self]
	end

	def to_Routput
		(RVector.new [self]).to_Routput
	end

end

class Array
	def to_Routput
		(RVector.new self).to_Routput
	end
end

def c(*vals)
	vals=vals.map{|e|
		if e.is_a? RVector
			e.to_a
		else
			e
		end
	}
  y=RVector.new vals.flatten
  return y;
end

def runif(n,from=0,to=1)
  yy=Distribution.new("unif",[from, to])
  return yy.sample(n);
end

def dunif(x,from=0,to=1)
  yy=Distribution.new("unif",[from, to])
  return yy.pdf(x);
end

def qunif(p,from=0,to=1)
  yy=Distribution.new("unif",[from, to])
  return yy.quantile(p);
end

def punif(q,from=0,to=1)
  yy=Distribution.new("unif",[from, to])
  return yy.cdf(q);
end

def rnorm(n,mu=0,sigma=1)
  yy=Distribution.new("norm",[mu, sigma])
  return yy.sample(n);
end

def dnorm(x,mu=0,sigma=1)
  yy=Distribution.new("norm",[mu, sigma])
  return yy.pdf(x);
end

def qnorm(p,mu=0,sigma=1)
  yy=Distribution.new("norm",[mu, sigma])
  return yy.quantile(p);
end

def pnorm(q,mu=0,sigma=1)
  yy=Distribution.new("norm",[mu, sigma]);
  return yy.cdf(q);
end

def rt(n,df)
  yy=Distribution.new("t",[df])
  return yy.sample(n);
end

def dt(x,df)
  yy=Distribution.new("t",[df])
  return yy.pdf(x);
end

def qt(p,df)
  yy=Distribution.new("t",[df]);
  return yy.quantile(p);
end

def pt(q,df)
  yy=Distribution.new("t",[df]);
  return yy.cdf(q);
end

def rchisq(n,df)
  yy=Distribution.new("chisq",[df])
  return yy.sample(n);
end

def dchisq(x,df)
  yy=Distribution.new("chisq",[df])
  return yy.pdf(x);
end

def qchisq(p,df)
  yy=Distribution.new("chisq",[df]);
  return yy.quantile(p);
end

def pchisq(q,df)
  yy=Distribution.new("chisq",[df]);
  return yy.cdf(q);
end

def rf(n,df1,df2)
  yy=Distribution.new("f",[df1,df2])
  return yy.sample(n);
end

def df(x,df1,df2)
  yy=Distribution.new("f",[df1,df2])
  return yy.pdf(x);
end

def qf(p,df1,df2)
  yy=Distribution.new("f",[df1,df2]);
  return yy.quantile(p);
end

def pf(q,df1,df2)
  yy=Distribution.new("f",[df1,df2]);
  return yy.cdf(q);
end

def rep(yy,nn)
	return RVector.new (yy.to_a * nn)
end

def seq(from,to)
	return RVector.new(from.upto(to).to_a)
end

def length(yy)
  return yy.size;
end

def range(yy)
	return RVector.new yy.range
end

def diff(yy)
	return RVector.new yy.diff
end

def sum(yy)
	yy.sum
end

def prod(yy)
	yy.prod
end

def cumsum(yy)
	return RVector.new yy.cumsum
end

def cumprod(yy)
	return RVector.new yy.cumprod
end

def cummin(yy)
	return RVector.new yy.cummin
end

def cummax(yy)
	return RVector.new yy.cummax
end

def mean(yy)
  yy.mean
end

def var(yy)
  yy.var
end

def varPop(yy)
	mean(yy**2) - mean(yy)**2
end

def sd(yy)
  yy.sd
end

def cov(yy,yy2)
	(mean(yy * yy2) - mean(yy)*mean(yy2))*yy.size/(yy.size-1)
end

def corr(yy,yy2)
	cov(yy,yy2)/sqrt(var(yy)*var(yy2))
end

def sqrt(x)
	(x.is_a? RVector) ? x.sqrt : Math.sqrt(x)
end

def quantile(yy,p)
	yy.quantile(p)
end

def seMean(yy)
  yy.seMean
end

def seVar(yy)
  yy.seVar
end

def seDMean(yy1,yy2,rho=1.0)
  Math.sqrt(yy1.var/yy1.size + rho**2 * yy2.var/yy2.size)
end

def seDMeanG(yy1,yy2)
	n1=yy1.size
	n2=yy2.size
	Math.sqrt(((n1 - 1) * yy1.var + (n2 - 1) * yy2.var)/(n1 + n2 - 2) * (1/n1 + 1/n2))
end

def seDVar(yy1,yy2,rho=1.0)
	yy1var=yy1.squareFromMean.var
	yy2var=yy2.squareFromMean.var
	Math.sqrt(yy1var/yy1.size + rho**2 * yy2var/yy2.size)
end

def seRMean(yy1,yy2,r0=nil)
	r0 ||= yy1.mean/yy2.mean
  Math.sqrt(yy1.var/yy1.size + r0**2 * yy2.var/yy2.size)/yy2.mean.abs
end

def seRVar(yy1,yy2,r0=nil)
	r0 ||= yy1.var/yy2.var
	yy1var=yy1.squareFromMean.var
	yy2var=yy2.squareFromMean.var
	Math.sqrt(yy1var/yy1.size + r0**2 * yy2var/yy2.size)/yy2.var
end

########################################
## Evaluation code 
########################################

def data_exo_old(exo)
	`getExoData(exo)`
end

def data_exo(exo)
	code=exo
		case exo 
		when "alfredN20"
			code="yA=c(0.144679564279082, 0.308391019822987, 0.165071844320451, 0.0810088511346089,-0.150489835962709, -0.0216344580330839, -0.255587942544298,0.0987153563164697, 0.592751352277294, -0.229624997435348, -0.216767323099697,-0.0970720818772463, 0.170505292061653, -0.0573236584450232,0.653375135601917, 0.178024688692504, 0.292787345192972, -0.165149721585414,-0.300802214354508, 0.32129751773001)"
		when "alfred"
			code="yA=c(0.144679564279082, 0.308391019822987, 0.165071844320451, 0.0810088511346089,-0.150489835962709, -0.0216344580330839, -0.255587942544298,0.0987153563164697, 0.592751352277294, -0.229624997435348, -0.216767323099697,-0.0970720818772463, 0.170505292061653, -0.0573236584450232,0.653375135601917, 0.178024688692504, 0.292787345192972, -0.165149721585414,-0.300802214354508, 0.32129751773001, 0.0805886239932639, 0.143171545682753,-0.00298066490980145, -0.255637645785468, 0.0928007482527018,-0.153861013155069, -0.091326084306205, 0.296172089667303, 0.00686887343119331,-0.596877687513186, -0.0584200940407693, 0.0720811015026169,0.0175687232410191, 0.153389781110694, -0.44979678290783, 0.213281328016978,-0.0386525306111942, -0.081768580854734, -0.165385713923966,-0.454948636494848, 0.262084533688519, -0.215753300244686, 0.173626814513985,-0.20016068068908, -0.255138748345693, 0.125329351017599, -0.326049545254994,0.207517749735126, 0.0704389198591811, -0.303221493327114)"
		when "dictee"
			code="yD=c(9, 10, 0, 1, 0, 5, 6, 10, 8, 1, 13, 9, 8, 3, 0, 0, 1, 0, 0, 0, 6, 9, 6, 8, 3, 5, 11, 5, 0, 0)"
		 when "dieteticien"
			code="yD=c(-1, 6, 4, 7, 7, 3, 2, 6, 3, 5, 5, 7, 4, 4, 2, 4, 6, 6, 5, 3, 5, 5, 2, 7, 4, 5, 4, 3, 6, 7, 4, 6, 4, 5, 2, 6, 4, 6, 5, 5, 6, 4, 3, 2, 3, 6, 5, 7, 2, 4);AV=c(64,67,68,76,72,69,62,65,64,73);AP=c(65,61,64,69,65,66,60,59,61,68)"
		when "produitA"
				code="yA=c(0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,1,0,0,1,0,0,0,0,1,0,1,0,1,0,0,0,1,1,0,1,0,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,1,1,0,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,0,1,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,0,0,0,1,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,1,0,1,1,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,1,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,1,0,1,0,0,0,0)"
	when "produitB"
				code="yB=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,2,0,0,2,0,0,2,0,0,0,2,0,2,2,2,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,2,3,0,0,0,0,0,3,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,3,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,0,0,0,0,0,0,0,3,0,1,0,2,0,0,0,0,2,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,3,0,0,1,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,2,0,0,0,0,0,2,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,1,0,1,0,2,0,0,0,0,0,0,0,0,0,0,2,0,0,0,2,0,3,0,0,0,0,0,1,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,2,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)"
		when "primeProdA"
				code="yQ=c(9,4,3,7,6,8,2,7,3,9,9,4,8,9,4,8,10,4,9,5,2,7,2,3,2,4,9,6,10,8,5,5,5,5,10,7,4,4,4,4,6,8,2,8,9,5,7,8,6,4,8,6,6,5,3,7,4,5,2,4,6,8,3,9,5,5,9,5,10,5,4,4,4,2,6,8,2,5,10,8,6,9,8,5,7,10,6,8,7,7,7,3,7,8,6,9,6,6,8,7,8,7,8,10,6,8,5,9,4,10,7,9,4,7,4,7,10,4,2,5,7,3,5,8,7,9,6,3,6,2,10,9,10,10,10,4,6,7,9,10,3,10,3,2,9,5,6,9,8,9,7,10,6,5,4,9,5,6,4,2,6,7,5,6,10,8,6,5,9,7,2,2,2,8,9,6,3,8,7,6,3,8,10,2,2,8,9,10,9,8,2,7,7,10,3,3,2,9,7,6)"
	when "competence"
				code="yC=c(0.3452062,0.3618771,0.2832608,-0.04273267,0.07897429,-0.5758346,-0.7199432,0.181882,0.04438047,-0.01951828,-0.3482005,0.2606782,-0.211132,-0.288865,-0.1782186,0.08062292,0.5697587,0.2163659,-0.05521338,-0.4348432,-0.08501561,0.05719431,-0.1577344,0.08525453,0.3121257,0.1269254,-0.178035,-0.1786163,-0.4814323,-0.07185419)"
	when "pilote1N20"
				code="y1=c(47.89674,50.04087,54.5324,54.36718,48.80645,51.44077,49.72669,44.81843,50.3791,48.32037,53.5515,50.38611,50.37899,49.44585,50.47262,50.66881,50.44251,50.10745,47.43873,51.17308)"
	when "pilote1"
				code="y1=c(47.89674,50.04087,54.5324,54.36718,48.80645,51.44077,49.72669,44.81843,50.3791,48.32037,53.5515,50.38611,50.37899,49.44585,50.47262,50.66881,50.44251,50.10745,47.43873,51.17308,51.1604,50.06874,48.85899,52.08231,49.59702,52.87073,50.63278,50.04054,53.13216,50.13489,53.72466,50.48863,48.83423,51.93978,49.19886,52.67034,49.2136,48.35678,49.43116,48.95199)"
		when "pilote2"
				code="y2=c(51.89371,51.35814,52.16305,51.83228,52.97653,51.43513,50.8937,51.50756,51.54468,52.22917,51.21122,52.96252,51.61797,52.40225,50.21097,51.73468,52.1436,52.57952,51.60721,52.15696,51.51422,53.23509,53.55053,52.62854,51.3968,52.51582,51.7682,53.42508,52.59148,51.55015,53.02435,52.20928,52.13902,52.04843,53.60312,50.48255,51.36794,52.73886,50.4655,51.92149,50.82357,53.11825,52.30076,49.99984,52.47019,51.9189,52.68014,51.25526,53.55974,52.44708)"
	when "bernardN20"
				code="yB=c(0.02483016,0.09379133,0.145188,-0.04969999,-0.1532143,0.1208757,-0.112933,-0.3452917,-0.007106278,0.1220161,-0.1919765,-0.3684244,0.1882093,-0.1190619,-0.2020528,0.2495189,-0.3013964,0.1123033,0.2164801,0.2393351)"
	when "bernard"
				code="yB=c(0.02483016,0.09379133,0.145188,-0.04969999,-0.1532143,0.1208757,-0.112933,-0.3452917,-0.007106278,0.1220161,-0.1919765,-0.3684244,0.1882093,-0.1190619,-0.2020528,0.2495189,-0.3013964,0.1123033,0.2164801,0.2393351,0.1428402,-0.323352,0.04930722,-0.02417904,0.1530558,0.1237836,-0.006933568,-0.06470596,0.1438609,0.02115967,-0.1333253,0.3044679,0.04528947,0.06454683,0.03938363,0.4622106,-0.1950227,-0.009857142,0.08429325,-0.04374064,-0.278324,0.3768047,-0.1410857,-0.01711132,0.04389444,0.07555481,0.2506217,0.3180829,0.005132367,0.0152481,0.2962752,-0.1621414,0.06079287,0.09097692,0.1198565,0.310566,-0.1467855,0.06196827,-0.1821278,-0.1878903)"
	when "conduite"
				code="yH=c(24,28,29,29,34,36,40,41,60);yF=c(21,31,34,37,38,39,42,43,44,50,51)"	
	when "adjverbe"
				code="sc=c(1.04,0.93,0.75,0.33,1.62,0.76,0.97,1.21,0.8,1.18);litt=c(1.32,2.3,1.98,0.59,1.02,0.88,0.92,1.39,1.95,1.25)"
		when "machine"
				code="yM1=c(1844,1828,1837,1833,1831,1818,1836,1837,1840,1820,1845,1815,1831,1839,1824,1839,1836,1840,1822,1824,1820,1839,1849,1846,1817,1822,1832,1846,1832,1834,1847,1828,1809,1833,1830,1824,1834,1842,1837,1818,1812,1825,1839,1840,1817,1827,1827,1842,1846,1839,1822,1816,1834,1810,1826,1836,1834,1839,1832,1827,1847,1842,1844,1831,1840,1823,1828,1822,1804,1821,1823,1846,1836,1823,1831,1810,1838,1844,1830,1830,1829,1807,1797,1814,1807,1844,1834,1827,1841,1830,1830,1834,1840,1832,1844,1815,1825,1821,1840,1821);yM2=c(2025,2045,2017,2024,2016,2025,2023,2020,2008,2025,2017,2014,2024,2028,2009,2023,2024,2034,2023,2024,2029,2032,2013,2017,2019,2022,2023,2005,2031,2012,2014,2032,2018,2022,2035,2024,2034,2012,2017,2015,2020,2015,2018,2020,2033,2025,2026,2026,2023,2014)"
	when "menu"
				code="yAV=c(8,7,6,7,9,7,6,4,7,5,8,7,6,6,6,6,7,5,7,6,7,7,8,5,4,7,6,5,7,6,8,6,7,7,7,8,5,8,5,5);yAP1=c(9,9,9,6,7,7,9,6,8,6,10,11,6,9,8,8,6,10,7,10,6,8,7,8,9,6,9,9,6,7);yAP2=c(8,10,8,8,9,8,9,5,10,6,10,7,7,6,6,7,11,7,12,6,7,7,10,6,7,7,7,5,8,8,10,6,9,8,8,9,5,10,7,8)"
	when "notes"
				code="yContC=c(16,16,12,16,11,10,15,19,12,14,17,16,12,10,17,16,12,14,11,16,18,20,13,14,9,10,18,7,6,10,17,18,11,17,14,16,12,12,13,13,12,16,11,15,19,14,16,13,17,18);yExamC=c(14,17,15,13,13,13,12,16,13,15,12,14,13,15,17,15,17,13,13,12,15,14,12,13,9,10,16,11,16,13,13,14,13,15,11,16,14,10,8,15,10,12,12,12,15,10,15,9,13,11);yExamD=c(13,13,11,10,13,11,12,11,9,13,10,11,12,14,11,11,10,17,10,7,17,11,9,10,14,11,9,11,10,10,12,11,12,12,10,9,12,12,10,11,8,5,14,9,12,11,11,9,11,11)"
	when "chAffN20"
				code="yP1=c(98.83,96.56,86.08,84.08,93.68,106.74,93.42,104.04,99.24,87.47,117.65,115.26,109.33,92.71,105.48,93.09,106.59,82.92,96.31,87.99);yP2=c(63.89,72.36,88.48,74.28,71.63,82.45,67.42,76.01,74.33,77.81,71.67,72.38,80.33,77.67,67.29,73.98,65.97,76.65,74.02,88.96);"
	when "chAff"
				code="yP1=c(98.83,96.56,86.08,84.08,93.68,106.74,93.42,104.04,99.24,87.47,117.65,115.26,109.33,92.71,105.48,93.09,106.59,82.92,96.31,87.99,99.77,111.22,106.49,100.8,109.97,96.91,83.39,101.57,100.1,110.07,94.03,114.85,105.3,106.5,88.68,100.94,98.4,101.98,112.11,79.68);yP2=c(63.89,72.36,88.48,74.28,71.63,82.45,67.42,76.01,74.33,77.81,71.67,72.38,80.33,77.67,67.29,73.98,65.97,76.65,74.02,88.96,71.19,81.9,75.03,80.35,86.16,73.15,73.94,63.95,79.94,59.04,67.5,77.15,74.01,77.45,78.13,74.46,96.59,80,78.19,72.97)"
	when "chAff0405N20"
				code="y04=c(84.03,95.47,88.89,93.09,87.24,90,86.85,86.61,73.24,73.88,97.2,96.47,85.61,64.47,67.98,78.2,86.76,81.73,74.35,83.55);y05=c(98.83,96.56,86.08,84.08,93.68,106.74,93.42,104.04,99.24,87.47,117.65,115.26,109.33,92.71,105.48,93.09,106.59,82.92,96.31,87.99)"
	when "chAff0405"
				code="y04=c(84.03,95.47,88.89,93.09,87.24,90,86.85,86.61,73.24,73.88,97.2,96.47,85.61,64.47,67.98,78.2,86.76,81.73,74.35,83.55,85.15,76.67,87.75,84.52,104.08,72.72,101.8,87.52,86.61,89.96,76.96,95.11,70.88,89.79,87.29,83.36,73.73,79.94,91.97,100.07);y05=c(98.83,96.56,86.08,84.08,93.68,106.74,93.42,104.04,99.24,87.47,117.65,115.26,109.33,92.71,105.48,93.09,106.59,82.92,96.31,87.99,99.77,111.22,106.49,100.8,109.97,96.91,83.39,101.57,100.1,110.07,94.03,114.85,105.3,106.5,88.68,100.94,98.4,101.98,112.11,79.68)"
	end
		return code
end

def evalMiniR(r_code,r_data="",output=:R,pre_r_code="") # output=:R or :js
	pre_r_code = pre_r_code + ";" + r_data   unless r_data.empty?
	r_code = pre_r_code + ";" + r_code unless pre_r_code.empty?
	opal_code=preprocessRCode(r_code)
	opal_code="("+ opal_code+")." + (output == :R  ? "to_Routput" : "to_s")
	js_code=Opal.compile(opal_code,{irb: true})
	return `eval(js_code)`
end

def preprocessRCode(r_code) 
	r_code.gsub(/\<\-/,"=").gsub(/([^\d])(\.\d+)/,"$1"+"0"+"$2").gsub("^","**")
end

########################################
## QuizzTest code 
########################################

module QuizzTest

	def QuizzTest.score_value(value,expected,score=[1,0])
		(value == expected ? score[0] : score[1])
	end

	def QuizzTest.score_expression(expression,expected,score=[1,0])
		(expression.gsub(" ","") == expected) ? score[0] : score[1]
	end

	def QuizzTest.score_expression_words(expression,expected,score=[1,0])
		words=expression.split(/[\(\)]/).map{|e| e.strip}.select{|e| !e.empty?}
		expected_words=expected.split(/[\(\)]/).map{|e| e.strip}.select{|e| !e.empty?}
		n = expected_words.length + words.length
		[n - ((words - expected_words).length + (expected_words - words).length),n]  
	end

	def QuizzTest.score_expression_contains(expression,expected_patterns,score=[1,0])
		expr = expression.gsub(" ","") 
		[expected_patterns.map{|pattern| (expr.include? pattern) ? score[0] : score[1]}.sum,expected_patterns.length]
	end
end

class QuizzScoreMiniR
	@@quizzs={}
	@@ids=[]

	def QuizzScoreMiniR.register(id,rule,r_data="",pre_r_code="")
		@@ids << id
		@@quizzs[id]=self.new(r_data,pre_r_code)
		@@quizzs[id].parse_rule(rule)
	end

	def QuizzScoreMiniR.[](id)
		@@quizzs[id]
	end

	def QuizzScoreMiniR.ls
		@@ids
	end

		attr_accessor :expr, :val, :rval, :rules, :r_data, :pre_r_code

	def initialize(r_data="",pre_r_code="")
		@answer=""
		@r_data,@pre_r_code=r_data,pre_r_code
	end

	def get_rval(expr=@expr)
		begin
			rval=evalMiniR(expr,data_exo(@r_data),:R,@pre_r_code)
		rescue
			rval=""
		end
		rval
	end

	def get_val(expr=@expr)
		begin
			val=evalMiniR(expr,data_exo(@r_data),:js,@pre_r_code)
			`eval(val)`
		rescue
			val=-1 # to change
		end
	end

	def parse_rule(rule)
		@rule=rule
		@expr,rule_txt="",""
		lines=@rule.strip.split("\n").map{|line| line.split("|")}.flatten
		lines.each{|cmd| #
			if cmd.strip =~/^expr\: (.*)/
				@expr=$1.strip
			elsif cmd.strip =~/^rule(?:s)?\: (.*)/
				rule_txt=$1.strip
			end
		}
		@rval,@val,@rules=nil,nil,[]
		unless rule_txt.empty?
			@rules=rule_txt.split("&").map{|e| e.strip}
			@rval=get_rval
			@val=get_val
		end
	end

	def eval(expr)
		score=@rules.map{|rule| #
			case rule
			when "val"
				val=get_val(expr)
				QuizzTest.score_value(val,@val,score=[1,0])
			when "rval"
				val=get_rval(expr)
				QuizzTest.score_value(rval,@rval,score=[1,0])
			when "expr"
				QuizzTest.score_expression(expr,@expr,score=[1,0])
			when "words"
				QuizzTest.score_expression_words(expr,@expr,score=[1,0])
			end
		}
		score.map{|e| (e.is_a? Array) ? (e[0].to_s + "/" + e[1].to_s) : e.to_s }.join("+")
	end
end