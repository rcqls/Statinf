/*
JavaScript file for Virtual Laboratories in Probability and Statistics, http://www.math.uah.edu/stat
*/

//Constants

const DISC = 0, CONT = 1, EULER = 0.5772156649, PRECINTEGER=5;

//Special Functions

//Error function
erf = function(x){
	var p = 0.3275911, a1 = 0.254829592, a2 = -0.284496736, a3 = 1.421413741, a4 = -1.453152027, a5 = 1.061405429;
	var x0 = Math.abs(x);
	var t = 1/(1 + p * x0)
	var y = 1 - (a1 * t + a2 * Math.pow(t, 2) + a3 * Math.pow(t, 3) + a4 * Math.pow(t, 4) + a5 * Math.pow(t, 5)) * Math.exp(-Math.pow(x, 2));
	if (x >= 0) return y;
	else return -y;
}

//Standard normal CDF
stdNormalCDF = function(x){
	return 0.5 + 0.5 * erf(x/Math.sqrt(2));
}

//Tests for parity
function isEven(n){
    if (n % 2 === 0) return true;
	else return false;
}

function isOdd(n){
	if ((n - 1) % 2 === 0) return true;
	else return false;
}

//Sign functon
function sgn(x){
	if (x > 0) return 1;
	else if (x < 0) return -1;
	else return 0;
}

//Sorting functions
function ascend(a, b){
	return a - b;
}

function descend(a, b){
	return b - a;
}

//Generalzied power function
function genPow(a, n, b){
	var p = 1;
	for (var i = 0; i < n; i++)	p = p * (a + i * b);
	return p;
}

//Rising power funtion
function risePow(a, n){
	return genPow(a, n, 1);
}

//Falling power function
function perm(n, k){
	var p = 1;
	for (var i = 0; i < k; i++)	p = p * (n - i);
	return p;
}

//Factorial function
function factorial(n){
	return perm(n, n);
}

//Binomial coefficient
function binomial(n, k){
	if (k < 0 || k > n) return 0;
	else{
		var p = 1;
		for (var i = 0; i < k; i++)	p = p * ((n - i)/(k - i));
		return p;
	}
}

//Polylogarithm function
function polyLog(a, x){
	var sum = 0, k = 1, e = 0.0001;
	while (Math.pow(x, k)/Math.pow(k, a) > e){
		sum = sum + Math.pow(x, k)/Math.pow(k, a);
		k++;
	}
	return sum;
}

//Random sample of size n from a population array p
function sample(p, n, type){
	var m = p.length;
	var t, k, u;
	var s = new Array(n);
	if (type == 1){
		for (var i = 0; i < n; i++){
			u = Math.floor(m * Math.random());
			s[i] = p[u];
		}
	}
	else{
		for (var j = 0; j < n; j++){
			k = m - j;
			u = Math.floor(k * Math.random());
			s[j] = p[u];
			t = p[k - 1];
			p[k - 1] = p[u];
			p[u] = t;
		}
	}
	return s;
}

//Log gamma function
function logGamma(x){
	var coef = [76.18009173, -86.50532033, 24.01409822, -1.231739516, 0.00120858003, -0.00000536382];
	var step = 2.50662827465, fpf = 5.5, t, tmp, ser;
	t = x - 1;
	tmp = t + fpf;
	tmp = (t + 0.5) * Math.log(tmp) - tmp;
	ser = 1;
	for (var i = 1; i <= 6; i++){
		t = t + 1;
		ser = ser + coef[i - 1]/t;
	}
	return tmp + Math.log(step * ser);
}

//Gamma function
function gamma(x){
	return Math.exp(logGamma(x));
}

//Gamma series function
function gammaSeries(x, a){
	var maxit = 100, eps = 0.0000003;
	var sum = 1/a, ap = a, gln = logGamma(a), del = sum;
	for (var n = 1; n <= maxit; n++){
		ap++;
		del = del * x/ap;
		sum = sum + del;
		if (Math.abs(del) < Math.abs(sum) * eps) break;
	}
	return sum * Math.exp(-x + a * Math.log(x) - gln);
}

//Gamma continued fraction function
function gammaCF(x, a){
	var maxit = 100, eps = 0.0000003;
	var gln = logGamma(a), g = 0, gOld = 0, a0 = 1, a1 = x, b0 = 0, b1 = 1, fac = 1;
	var an, ana, anf;
	for (var n = 1; n <= maxit; n++){
		an = 1.0 * n;
		ana = an - a;
		a0 = (a1 + a0 * ana) * fac;
		b0 = (b1 + b0 * ana) * fac;
		anf = an * fac;
		a1 = x * a0 + anf * a1;
		b1 = x * b0 + anf * b1;
		if (a1 !== 0){
			fac = 1.0/a1;
			g = b1 * fac;
			if (Math.abs((g - gOld)/g) < eps) break;
			gOld = g;
		}
	}
	return Math.exp(-x + a * Math.log(x) - gln) * g;
}

//Gamma CDF
function gammaCDF(x, a){
	if (x <= 0) return 0;
	else if (x < a + 1) return gammaSeries(x, a);
	else return 1 - gammaCF(x, a);
}

//Beta continued fraction function
	function betaCF(x, a, b){
	var maxit = 100, eps = 0.0000003, am = 1, bm = 1, az = 1, qab = a + b, qap = a + 1, qam = a - 1, bz = 1 - qab * x/qap, tem, em, d, bpp, bp, app, aOld, ap;
	for (var m = 1; m <= maxit; m++){
		em = m;
		tem = em + em;
		d = em * (b - m) * x/((qam + tem) * (a + tem));
		ap = az + d * am;
		bp = bz + d * bm;
		d = -(a + em) *(qab + em) * x/((a + tem) * (qap + tem));
		app = ap + d * az;
		bpp = bp + d * bz;
		aOld = az;
		am = ap/bpp;
		bm = bp/bpp;
		az = app/bpp;
		bz = 1;
		if (Math.abs(az - aOld) < eps * Math.abs(az)) break;
	}
	return az;
}

//Beta CDF
function betaCDF(x, a, b){
	var bt;
	if ((x === 0) || (x === 1)) bt = 0;
	else bt = Math.exp(logGamma(a + b) - logGamma(a) - logGamma(b) + a * Math.log(x) + b * Math.log(1 - x));
	if (x < (a + 1)/(a + b + 2)) return bt * betaCF(x, a, b)/a;
	else return 1 - bt * betaCF(1 - x, b, a)/b;
}

//Zeta function
function zeta(s){
	var digits = 4;
	var terms = Math.ceil(Math.pow(10, digits/s));
	var sum = 0;
	for (var n = 1; n < terms; n++)
	sum = sum + 1/Math.pow(n, s);
	return sum;
}

//Interval data distribution taking values in the interval [a, b], with step size t
function Data(a0, b0, t0){
	var a, b, t;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (a <= b0 && b0 < Infinity) b = b0; else b = a + 1;
	if (0 < t0 && t0 <= b - a) t = t0; else t = 1;

	var n = 0, s = 0, ss = 0, xl, xu;
	//number of values, sum, sum of squares, min value, max value
	var m = Math.round((b - a)/t) + 1, f = new Array(m);
	for (var i = 0; i < m; i++) f[i] = 0;

	//Methods
	this.lower = function(){
		return a;
	};

	this.upper = function(){
		return b;
	};

	this.step = function(){
		return t;
	};

	this.index = function(x){
		return Math.round((x - a)/t);
	};

	this.setValue = function(x){
		n++;
		s = s + x;
		ss = ss + x * x;
		if (x < xl) xl = x;
		if (x > xu) xu = x;
		f[this.index(x)]++;
	};

	this.mean = function(){
		return s/n;
	};

	this.meanSquare = function(){
		return ss/n;
	};

	this.variance = function(){
		return ss/(n - 1) - s * s /(n * (n - 1));
	};

	this.stdDev = function(){
		return Math.sqrt(this.variance());
	};

	this.freq = function(x){
		return f[this.index(x)];
	};

	this.relFreq = function(x){
		return this.freq(x)/n;
	};

	this.density = function(x){
		return this.relFreq(x)/t;
	};

	this.size = function(){
		return n;
	};

	this.min = function(){
		return xl;
	};

	this.max = function(){
		return xu;
	};

	this.reset = function(){
		s = 0; ss = 0; n = 0;
		xl = b; xu = a;
		for(i = 0; i < m; i++) f[i] = 0;
	};

	this.array = function(){
		return f;
	};
}

//Complete data distribution
function CompleteData(){
	var v = new Array(), s = 0, s2 = 0, n = 0;
	//Values, sum, sum of squares, number of values

	this.setValue = function(x){
		v.push(x);
		s = s + x;
		s2 = s2 + x * x;
		n++;
	};

	this.getValue = function(i){
		if (0 <= i && i < n) return v[i];
		else return NaN;
	};

	this.getValues = function(){
		return v;
	};

	this.size = function(){
		return n;
	};

	this.mean = function(){
		return s/n;
	};

	this.varianceP = function(){
		return s2/n - Math.pow(this.mean(), 2);
	};

	this.stdDevP = function(){
		return Math.sqrt(this.varianceP());
	};

	this.variance = function(){
		return (n/(n - 1)) * this.varianceP();
	};

	this.stdDev = function(){
		return Math.sqrt(this.variance());
	};

	this.reset = function(){
		v = new Array();
		s = 0; s2 = 0; n = 0;
	};

	this.orderStat = function(i){
		var v1 = v;
		v1.sort(ascend)
		return v1[i - 1];
	};

	this.quantile = function(p0){
		var p, r, k, t;
		if (0 <= p0 && p0 <= 1) p = p0; else p = 0.5;
		r = (n - 1) * p + 1;
		k = Math.floor(r);
		t = r - k;
		if (k == n) return this.orderStat(k);
		else return this.orderStat(k) + t * (this.orderStat(k + 1) - this.orderStat(k));
	};

	this.minValue = function(){
		return this.orderStat(1);
	};

	this.maxValue = function(){
		return this.orderStat(n);
	};

	this.median = function(){
		return this.oderStat(0.5);
	};

	this.quartile = function(i){
		if (i == 1) return this.orderStat(0.25);
		else if (i == 2) return this.orderStat(0.5);
		else if (i == 3) return this.orderStat(0.75)
		else return NaN;
	};

	this.freq = function(a, b){
		var k = 0;
		for (var i = 0; i < n; i++) if (a <= v[i] && v[i] <= b) k++;
		return k;
	};

	this.relFreq = function(a, b){
		return this.freq(a, b)/n;
	};

	this.density = function(a, b){
		return this.relFreq(a, b)/(b - a);
	};
}

//Generic probability distribution on the interval [a, b] with step size s of a give type with a given pdf
function Distribution(a0, b0, s0, type0, pdf0){
	var a, b, s, type, pdf, n, data, dx;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (a <= b0 && b0 < Infinity) b = b0; else b = 1;
	if (0 < s0 && s0 <= b - a) s = s0; else s = 1;
	if (type0 === DISC || type0 === CONT) type = type0; else type = DISC;
	n = Math.round((b - a)/s) + 1;
	if (pdf0 instanceof Array) pdf = pdf0;

	if (type === DISC) dx = 1; else dx = s;

	data = new Data(a, b, s);

	this.minValue = function(){
		return a;
	};

	this.maxValue = function(){
		return b;
	}

	this.step = function(){
		return s;
	}

	this.type = function(){
		return type;
	};

	this.data = function(){
		return data;
	}

	this.density = function(x0){
		if (a <= x0 && x0 <= b) return pdf[Math.round((x0 - a)/s)];
		else return 0;
	};

	this.mode = function(){
		var x0 = a, y0 = this.density(x0), y;
		for (var x = a; x <= b; x = x + s){
			y = this.density(x);
			if (y > y0){
				y0 = y;
				x0 = x;
			}
		}
		return x0;
	};

	this.maxDensity = function(){
		return this.density(this.mode());
	};

	this.CDF = function(y){
		if (y < a) return 0;
		else if (y >= b) return 1;
		else {
			var sum = 0;
			for (var x = a; x <= y; x = x + s) sum = sum + this.density(x) * dx;
			return sum;
		};
	};

	this.quantile = function(p){
		var x, q, k, x1, x2, e;
		if (p === 0) return a;
		else if (p === 1) return b;
		else if (0 < p & p < 1){
			if (type === 0){
				x = a;
				q = this.density(x);
				while(q < p){
					x = x + s;
					q = q + this.density(x);
				}
			}
			else {
				x1 = a; x2 = b;
				x = (x1 + x2)/2;
				q = this.CDF(x);
				e = Math.abs(q - p);
				k = 1;
				while (e > 0.00001 && k < 100){
					k++;
					if (q < p) x1 = x; else x2 = x;
					x = (x1 + x2)/2;
					q = this.CDF(x);
					error = Math.abs(q - p);
				}
			}
			return x;
		}
		else return NaN;
	};

	this.moment = function(k, t){
		var sum = 0;
		for (var x = a; x <= b; x = x + s) sum = sum + Math.pow((x - t), k) * this.density(x) * dx;
		return sum;
	};

	this.rawMoment = function(k){
		return this.moment(k, 0);
	};

	this.mean = function(){
		return this.rawMoment(1);
	};

	this.centralMoment = function(k){
		return this.moment(k, this.mean());
	};

	this.variance = function(){
		return this.centralMoment(2);
	};

	this.stdDev = function(){
		return Math.sqrt(this.variance());
	};

	this.skew = function(){
		return this.centralMoment(3)/Math.pow(this.stdDev(), 3);
	};

	this.kurt = function(){
		return this.centralMoment(4)/Math.pow(this.stdDev(), 4);
	};

	this.MGF = function(t){
		var sum = 0;
		for (var x = a; x <= b; x = x + s) sum = sum + Math.exp(t * x) * this.density(x) * dx;
		return sum;
	};

	this.PGF = function(t){
		var sum = 0;
		for (var x = a; x <= b; x = x + s) sum = sum + Math.pow(t, x) * this.density(x) * dx;
		return sum;
	};

	this.median = function(){
		return this.quantile(0.5);
	};

	this.quartile = function(i){
		if (i == 1) return this.quantile(0.25);
		else if (i == 2) return this.quantile(0.5);
		else if (i == 3) return this.quantile(0.75);
		else return NaN;
	};

	this.simulate = function(){
		var x = this.quantile(Math.random());
		this.setValue(x);
		return x;
	};

	this.setValue = function(x){
		data.setValue(x);
	};

	// Added by me
	this.isValue = function(x) {
		var xx = (x - this.minValue())/(this.step());
		//console.log(["isValue",x,xx]);
		return xx.toFixed(PRECINTEGER) == Math.round(xx).toFixed(PRECINTEGER); //An integer with 7 decimals
	};

	this.values = function() {
		var arr = [],current = this.minValue(),cnt = 0;
		//arr.push(this.minValue());
		for (; current < this.maxValue(); cnt++, current = this.minValue() + this.step() * cnt) {
			//console.log(["val",current,this.isValue(current)])
			 if(this.isValue(current)) arr.push(current);
		}
		arr.push(this.maxValue());
		return arr;
	}

	this.regular = function() {
		return true;
	}
}

//Point mass distribution at a given point

function PointMassDistribution(c0){
	var c;
	if (-Infinity < c0 && c0 < Infinity) c = c0; else c = 0;
	Distribution.call(this, c, c, 1, DISC);

	this.density = function(x){
		if (x == c) return 1;
		else return 0;
	};

	this.mode = function(){
		return c;
	};

	this.mean = function(){
		return c;
	};

	this.variance = function(){
		return 0;
	};

	this.CDF = function(x){
		if (x < c) return 0;
		else if (x >= c) return 1;
		else return NaN;
	};

	this.quantile = function(p){
		if (0 <= p && p <= 1) return c;
		else return NaN;
	};

	this.simulate = function(){
		return c;
	};

	this.PGF = function(t){
		return Math.pow(t, c);
	};
}
PointMassDistribution.prototype = new Distribution;

//Distributions related to Bernoulli trials

//Binomial distribution with n trials and probability of success p
function BinomialDistribution(n0, p0){
	var n, p;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	if (0 <= p0 && p0 <= 1) p = p0; else p = 0.5;
	Distribution.call(this, 0, n, 1, DISC);

	this.density = function(x){
		if (0 <= x && x <= n){
			var k = Math.round(x);
			return binomial(n, k) * Math.pow(p, k) * Math.pow(1 - p, n - k);
		}
		else return 0;
	};

	this.mode = function(){
		if (p == 1) return n;
		else return Math.floor((n + 1) * p);
	};

	this.mean = function(){
		return n * p;
	};

	this.variance = function(){
		return n * p * (1 - p);
	};

	this.simulate = function(){
		var k = 0;
		for (var i = 1; i <= n; i++) if (Math.random() < p) k++;
		this.setValue(k);
		return k;
	};

	this.PGF = function(t){
		return Math.pow(1 - p + p * t, n);
	};

	this.MGF = function(t){
		return this.PGF(Math.exp(t));
	};

	this.skew = function(){
		return (1 - 2 * p)/Math.sqrt(n * p * (1 - p));
	};

	this.kurt = function(){
		return (1 - 6 * p * (1 - p))/(n *p * (1 - p));
	};

	this.factorialMoment = function(k){
		return perm(n, k) * Math.pow(p, k);
	};

	this.trials = function(){
		return n;
	};

	this.prob = function(){
		return p;
	};
}
BinomialDistribution.prototype = new Distribution;

//Bernoulli distribution with parameter p
function BernoulliDistribution(p0){
	BinomialDistribution.call(this, 1, p0);
}
BernoulliDistribution.prototype = new BinomialDistribution;

//Negative binomial distribution with stopping parameter k and success probability p
function NegativeBinomialDistribution(k0, p0){
	var k, p;
	if (1 <= k0 && k0 < Infinity) k = k0; else k = 1;
	if (0 < p0 && p0 <= 1) p = p0; else p = 0.5;
	var m = k/p, v = k * (1 - p)/Math.pow(p, 2);
	Distribution.call(this, k, Math.round(m + 4 * Math.sqrt(v)), 1, DISC);

	this.mode = function(){
		return Math.floor((k - 1)/p + 1);
	};

	this.density = function(x){
		var n = Math.round(x);
		if (n < k) return 0;
		else return binomial(n - 1, k - 1) * Math.pow(p, k) * Math.pow(1 - p, n - k);
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return v;
	};

	this.simulate = function(){
		var count = 0, trials = 0;
		while (count < k){
			if (Math.random() < p) count++;
			trials++;
		}
		this.setValue(trials);
		return trials;
	};

	this.PGF = function(t){
		if (Math.abs(t) >= 1/(1 - p)) return NaN;
		else return Math.pow(p * t/(1 - (1 - p) * t), k);
	};

	this.MGF = function(t){
		return this.PGF(Math.exp(t));
	};

	this.skew = function(){
		return (2 - p)/Math.sqrt(k * (1 - p));
	};

	this.kurt = function(){
		return (1/k) * (6 + p * p/(1 - p));
	};

	this.stop = function(){
		return k;
	};

	this.prob = function(){
		return p;
	};
}
NegativeBinomialDistribution.prototype = new Distribution;

//Geometric distribution with success parameter p
function GeometricDistribution(p0){
	NegativeBinomialDistribution.call(this, 1, p0);

	this.CDF = function(x){
		return 1 - Math.pow(1 - this.prob(), x);
	}

	this.quantile = function(q){
		return Math.round(Math.log(1 - q)/Math.log(1 - p));
	}
}
GeometricDistribution.prototype = new NegativeBinomialDistribution;

//Beta-binomial distribution with left shape parameter a, right shape parameter b and n trials
function BetaBinomialDistribution(a0, b0, n0){
	var a, b, n;
	if (0 < a0 && a0 < Infinity) a = a0; else a = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	if (1 <= n0 & n0 < Infinity) n = Math.round(n0); else n = 1;
	Distribution.call(this, 0, n, 1, DISC);

	this.density = function(x){
		return binomial(n, x) * risePow(a, x) * risePow(b, n - x)/risePow(a + b, n);
	};


	this.mean = function(){
		return n * (a/(a + b));
	};

	this.variance = function(){
		return (n * a * b/Math.pow(a + b, 2)) * (1 + (n - 1)/(a + b + 1));
	};

	this.left = function(){
		return a;
	};

	this.right = function(){
		return b;
	};

	this.trials = function(){
		return n;
	};
}
BetaBinomialDistribution.prototype = new Distribution;

//Beta-negative binomial distribution with left shape parameter a, right shape parameter b, and k successes
function BetaNegativeBinomialDistribution(a0, b0, k0){
	var a, b, k, m, s2;
	if (0 < a0 && a0 < Infinity) a = a0; else a = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	if (1 <= k0 && k0 < Infinity) k = Math.round(k0); else k = 1;

	m = k * (a + b - 1)/(a - 1);
	s2 = k * (a + b - 1) * (b + k * (a + b - 2))/ ((a - 1) * (a - 2)) - Math.pow(m, 2);
	Distribution.call(this, k, Math.round(m + 4 * Math.sqrt(s2)), 1, DISC);

	this.density = function(x){
		var prod1 = 1, prod2 = 1;
		for (var i = 0; i < k; i++) prod1 = prod1 * (a + i)/(a + b + i);
		for (var j = 0; j < x - k; j++) prod2 = prod2 * (b + j)/(a + b + k + j);
		return binomial(x - 1, k - 1) * prod1 * prod2;
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return s2;
	};

	this.left = function(){
		return a;
	};

	this.right = function(){
		return b;
	};

	this.successes = function(){
		return k;
	};
}
BetaNegativeBinomialDistribution.prototype = new Distribution;

//The binomial distribution with the number of trials randomized
function BinomialNDistribution(d0, p0){
	var d, p;
	if (d0 instanceof Distribution) d = d0; else d = new PointMassDistribution(1);
	if (0 <= p0 && p0 <= 1) p = p0; else p = 0.5;
	Distribution.call(this, 0, d.maxValue(), 1, DISC);

	this.density = function(x){
		var sum = 0;
		for (var n = x; n <= d.maxValue(); n = n + d.step()) sum = sum + d.density(n) * binomial(n, x) * Math.pow(p, x) * Math.pow(1 - p, n - x);
		return sum;
	};

	this.mean = function(){
		return d.mean() * p
	};

	this.variance = function(){
		return d.mean() * p * (1 - p) + Math.pow(p, 2) * d.variance();
	};

	this.simulate = function(){
		var trials = Math.round(d.simulate());
		var successes = 0;
		for (var i = 0; i <= trials; i++) if (Math.random() <= p) successes++;
		this.setValue(successes);
		return successes;
	};

	this.dist = function(){
		return d;
	}

	this.prob = function(){
		return p;
	}
}
BinomialNDistribution.prototype = new Distribution;

//Distributions based on finite sampling models

//Hypergeometric distribution with population size m, number of red r, and sample size n
function HypergeometricDistribution(m0, r0, n0){
	var m, r, n;
	if (1 <= m0 && m0 < Infinity) m = Math.round(m0); else m = 10;
	if (1 <= r0 && r0 <= m) r = Math.round(r0); else r = 5;
	if (1 <= n0 && n0 <= m) n = Math.round(n0); else n = 5;
	Distribution.call(this, Math.max(0, n - (m - r)), Math.min(n, r), 1, DISC);

	this.density = function(x){
		var k = Math.round(x);
		return binomial(r, k) * binomial(m - r, n - k)/binomial(m, n);
	};

	this.mode = function(){
		return Math.floor((r + 1) * (n + 1)/(m + 2));
	};

	this.mean = function(){
		return n * (r/m);
	};

	this.variance = function(){
		return n * (r/m) * (1 - r/m) * (m - n)/(m - 1);
	};

	this.population = function(){
		return m;
	};

	this.red = function(){
		return r;
	};

	this.sample = function(){
		return n;
	};
}
HypergeometricDistribution.prototype = new Distribution;

//Polya distribution with r initial red, g initial green, a added, and sample size n
function PolyaDistribution(r0, g0, a0, n0){
	var r, g, a, n;
	if (1 <= r0 && r0 < Infinity) r = Math.round(r0); else r = 10;
	if (1 <= g0 && g0 < Infinity) g = Math.round(g0); else g = 10;
	if (0 <= a0 && a0 < Infinity) a = Math.round(a0); else a = 0;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 5;
	Distribution.call(this, 0, n, 1, DISC);

	//Methods
	this.density = function(x){
		var m = r + g;
		var p1 = 1, p2 = 1;
		for (var i = 0; i < x; i++) p1 = p1 * (n - i) * (r + i * a)/((m + i * a) * (x - i));
		for (var j = 0; j < n - x; j++) p2 = p2 * (g + j * a)/(m + (x + j) * a);
		return p1 * p2;
	};

	this.mean = function(){
		return n * r/(r + g);
	};

	this.variance = function(){
		var p = r/(r + g), q = a/(r + g + a);
		return n * p * (1 - p) * (1 + (n - 1) * q);
	};

	this.simulate = function(){
		var s = 0, rt = r, gt = g, p;
		for (var i = 1; i <= n; i++){
			p = rt/(rt + gt);
			if (Math.random() < p) {
				s++;
				rt = rt + a;
			}
			else gt = gt + a;
		}
		this.setValue(s);
		return s;
	};

	this.red = function(){
		return r;
	}

	this.green = function(){
		return g;
	};

	this.add = function(){
		return a;
	};

	this.trials = function(){
		return n;
	};

}
PolyaDistribution.prototype = new Distribution;

//Birthday Distribution with population size m and sample size n
function BirthdayDistribution(m0, n0){
	var m, n;
	if (1 <= m0 && m0 < Infinity) m = m0; else m = 10;
	if (1 <= n0 && n0 < Infinity) n = n0; else n = 5;
	Distribution.call(this, 1, Math.min(m, n), 1, DISC);

	var count = new Array(m);
	var upperIndex;
	var prob = new Array(n + 1);
	for (var i = 0; i < n + 1; i++){
		prob[i] = new Array(m + 1);
		for (var j = 0; j < m + 1; j++) prob[i][j] = 0;
	}
	prob[0][0] = 1; prob[1][1] = 1;
	for (var k = 1; k < n; k++){
		if (k < m) upperIndex = k + 1; else upperIndex = m;
		for (var l = 1; l <= upperIndex; l++){
			prob[k+1][l] = prob[k][l] * (l/m) + prob[k][l - 1] * ((m - l + 1)/m);
		}
	}

	var mode = 1;
	for (var x = 1; x <= Math.min(m, n); x++) if (prob[n][x] > prob[n][mode]) mode = x;

	this.density = function(x){
		return prob[n][x];
	};

	this.mode = function(){
		return mode;
	};

	this.mean = function(){
		return m * (1 - Math.pow(1 - 1/m, n));
	};

	this.variance = function(){
		return m * (m - 1) * Math.pow(1 - 2/m, n) + m * Math.pow(1 - 1/m, n) - m * m * Math.pow(1 - 1/m, 2 * n);
	};

	this.simulate = function(){
		for (var i = 0; i < m; i++) count[i] = 0;
		var distinct = 0;
		for (var i = 1; i <= n; i++){
			var j = Math.floor(m * Math.random());
			if (count[j] === 0) distinct++;
			count[j]++;
		}
		this.setValue(distinct);
		return distinct;
	};

	this.population = function(){
		return m;
	};

	this.sample = function(){
		return n;
	};
}
BirthdayDistribution.prototype = new Distribution;

//Coupon collector distribution with population size m and k distinct
function CouponDistribution(m0, k0){
	var m, k;
	if (1 <= m0 && m0 < Infinity) m = Math.round(m0); else m = 10;
	if (1 <= k0 && k0 <= m) k = Math.round(k0); else k = 5;
	var c = binomial(m - 1, k - 1);
	var mu = 0, s2 = 0;
	for (var i = 1; i <= k; i++) mu = mu + m/(m - i + 1);
	for (var i = 1; i <= k; i++) s2 = s2 + (m * (i - 1))/((m - i + 1) * (m - i + 1));
	Distribution.call(this, k, Math.round(mu + 4 * Math.sqrt(s2)), 1, DISC);

	this.density = function(x){
		var sum = 0;
		for (var j = 0; j < k; j++) sum = sum + Math.pow(-1, j) * binomial(k - 1, j) * Math.pow((k - j - 1)/m, x - 1);
		return c * sum;
	}

	this.mean = function(){
		return mu;
	};

	this.variance = function(){
		return s2;
	};

	this.simulate = function(){
		var cellCount = new Array(m);
		for (var i = 0; i < m; i++) cellCount[i] = 0;
		var occupiedCells = 0;
		var ballCount = 0;
		while (occupiedCells < k){
			ballCount++;
			var ballIndex = Math.floor(m * Math.random());
			if (cellCount[ballIndex] === 0) occupiedCells++;
			cellCount[ballIndex]++;
		}
		this.setValue(ballCount);
		return ballCount;
	};

	this.population = function(){
		return m;
	}

	this.distinct = function(){
		return k;
	};
}
CouponDistribution.prototype = new Distribution;


//Finite order statistic distribution with population size m, sample size n, and order k
function FiniteOrderStatistic(m0, n0, k0){
	var m, n, k;
	if (1 <= m0 && m0 < Infinity) m = Math.round(m0); else m = 10;
	if (1 <= n0 && n0 <= m) n = Math.round(n0); else n = 5;
	if (1 <= k0 && k0 <= n) k = Math.round(k0); else k = 1;
	Distribution.call(this, k, m - n + k, 1, DISC);

	this.density = function(x){
		return binomial(x - 1, k - 1) * binomial(m - x, n - k)/binomial(m, n);
	};

	this.mean = function(){
		return k * (m + 1)/(n + 1);
	};

	this.variance = function(){
		return (m + 1) * (m - n) * k * (n + 1 - k)/((n + 1) * (n + 1) * (n + 2));
	};

	this.population = function(){
		return m;
	};

	this.sample = function(){
		return n;
	};

	this.order = function(){
		return k;
	};
}
FiniteOrderStatistic.prototype = new Distribution;

//Matching distribtion with population size n
function MatchDistribution(n0){
	var n;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 10;
	Distribution.call(this, 0, n, 1, DISC);

	this.density = function(x){
		var sum = 0, sign = -1;
		for (var j = 0; j <= n - x; j++){
			sign = -sign;
			sum = sum + sign/factorial(j);
		}
		return sum/factorial(x);
	};

	this.mode = function(){
		if (n == 2) return 0;
		else return 1;
	};

	this.mean = function(){
		return 1;
	};

	this.variance = function(){
		return 1;
	};

	this.simulate = function(){
		var p = new Array(n), s = new Array(n), sum = 0;
		for (var i = 0; i < n; i++) p[i] = i + 1;
		s = sample(p, n);
		for (var j = 0; j < n; j++) if (s[j] == j + 1) sum++;
		this.setValue(sum);
		return sum;
	};

	this.population = function(){
		return n;
	}
}
MatchDistribution.prototype = new Distribution;

//Distributions related to random walks

//Distribution of the maximum position in the simple random walk with n steps
function WalkMaxDistribution(n0){
	var n;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 10;
	Distribution.call(this, 0, n, 1, DISC);

	this.density = function(x){
		var m;
		if ((x + n) % 2 === 0) m = (x + n)/2;
		else m = (x + n + 1)/2;
		return binomial(n, m)/Math.pow(2 , n);
	};

	this.maxDensity = function(){
		return this.density(0);
	};

	this.simulate = function(){
		var step, max = 0, position = 0;
		for (var i = 1; i <= n; i++){
			if (Math.random() < 0.5) step = 1;
			else step = -1;
			position = position + step;
			if (position > max) max = position;
		}
		this.setValue(max);
		return max;
	};
}
WalkMaxDistribution.prototype = new Distribution;

//Distribution of the final position in the simple random walk with n steps
function WalkPositionDistribution(n0, p0){
	var n, p;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 10;
	if (0 <= p0 && p0 <= 1) p = p0; else p = 0.5;
	Distribution.call(this, -n, n, 2, DISC);

	this.density = function(x){
		var k = Math.round((x + n)/2);
		return binomial(n, k) * Math.pow(p, k) * Math.pow(1 - p, n - k);
	};

	this.mode = function(){
		if (p == 1) return n;
		else return 2 * Math.floor((n + 1) * p) - n;
	};

	this.mean = function(){
		return n * (2 * p - 1);
	};

	this.variance = function(){
		return 4 * n * p * (1 - p);
	};

	this.simulate = function(){
		var step, position = 0;
		for (var i = 1; i <= n; i++){
			if (Math.random() < p) step = 1;
			else step = -1;
			position = position + step;
		}
		this.setValue(position);
		return position;
	};

	this.steps = function(){
		return n;
	};

	this.prob = function(){
		return p;
	};
}
WalkPositionDistribution.prototype = new Distribution;

//Discrete arcsine distribution with n steps
function DiscreteArcsineDistribution(n0){
	var n;
	if (2 <= n0 && isEven(n0)) n = n0; else n = 2 * Math.round(n0/2);
	Distribution.call(this, 0, n, 2, DISC);

	this.density = function(x){
		if (isEven(x) && x >= 0 && x <= n) return binomial(x, x/2) * binomial(n - x, (n - x)/2)/Math.pow(2, n);
		else return 0;
	};

	this.mode = function(){
		return 0;
	};

	this.mean = function(){
		return n/2;
	};

	this.simulate = function(){
		var step, lastZero = 0, position = 0;
		for (var i = 1; i <= n; i++){
			if (Math.random() < 0.5) step = 1;
			else step = -1;
			position = position + step;
			if (position === 0) lastZero = i;
		}
		this.setValue(lastZero);
		return lastZero;
	};

	this.steps = function(){
		return n;
	}
}
DiscreteArcsineDistribution.prototype = new Distribution;


//Distributions related to the normal distribution

//Normal distribution with mean m and standard deviation s
function NormalDistribution(m0, s0){
	var m, s, c;
	if (-Infinity < m0 && m0 < Infinity) m = m0; else m = 0;
	if (0 < s0 && s0 < Infinity) s = s0; else s = 1;
	Distribution.call(this, m - 4 * s, m + 4 * s, (4 * s)/25, CONT);

	c = 1/(s * Math.sqrt(2 * Math.PI));

	this.mode = function(){
		return m;
	};

	this.maxDensity = function(){
		return c;
	};

	this.density = function(x){
		var z = (x - m)/s;
		return c * Math.exp(-0.5 * Math.pow(z, 2));
	};

	this.CDF = function(x){
		return stdNormalCDF((x - m)/s);
	};

	this.simulate = function(){
		var r = Math.sqrt(-2 * Math.log(Math.random()));
		var theta = 2 * Math.PI * Math.random();
		var x = m + s * r * Math.cos(theta);
		this.setValue(x);
		return x;
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return Math.pow(s, 2);
	};

	this.stdDev = function(){
		return s;
	};

	this.MGF = function(t){
		return Math.exp(m + 0.5 * Math.pow(s, 2) * Math.pow(t, 2));
	};

	this.PGF = function(t){
		if (t <= 0) return NaN;
		else return this.MGF(Math.log(t));
	};

	this.centralMoment = function(n){
		if (isOdd(n)) return 0;
		else if (isEven(n)){
			var k = n/2;
			return (factorial(n)/(factorial(k) * Math.pow(2, k))) * Math.pow(s, n);
		}
	};
}
NormalDistribution.prototype = new Distribution;

//Folded Normal distribution with normal mean a and normal standard deviation b
function FoldedNormalDistribution(a0, b0){
	var a, b, m, s, s2, c, mx, mn;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;

	m = b * Math.sqrt(2/Math.PI) * Math.exp(-Math.pow(a, 2)/(2 * Math.pow(b,2))) + a * (1 - 2 * stdNormalCDF(-a/b));
	s2 = Math.pow(a, 2) + Math.pow(b, 2) - Math.pow(b * Math.sqrt(2/Math.PI) * Math.exp(-Math.pow(a, 2)/(2 * Math.pow(b,2))) + a * (1 - 2 * stdNormalCDF(-a/b)),2);
	s = Math.sqrt(s2);
	mn = Math.max(0, m - 3 * s); mx = m + 3 * s;
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	var c = 1/(b * Math.sqrt(2 * Math.PI));

    this.maxDensity = function(){
    	return Math.max(this.density(Math.abs(a)), this.density(0));
	}

    this.density = function(x){
		var z = -1/(2 * b * b);
        if (x >= 0) return c * Math.exp(z * (-x - a) * (-x - a)) + c * Math.exp(z * (x - a) * (x - a));
        else return 0;
     }

     this.CDF = function(x){
		return 0.5 * (erf((x + a)/(Math.sqrt(2) * b)) + erf((x - a)/(Math.sqrt(2) * b)));
	}

	this.mean = function(){
	  return m;
	}

    this.variance = function(){
		return s2;
	}

	this.simulate = function(){
		var r = Math.sqrt(-2 * Math.log(Math.random()));
		var theta = 2 * Math.PI * Math.random();
		var x = Math.abs(a + b * r * Math.cos(theta));
        this.setValue(x);
		return x;
	}

	this.normalMean = function(){
		return a;
	};

	this.normalStdDev = function(){
		return b;
	};
}
FoldedNormalDistribution.prototype = new Distribution;

//Half normal distribution with scale parameter b
function HalfNormalDistribution(b0){
	FoldedNormalDistribution.call(this, 0, b0);

	this.mode = function(){
		return 0;
	}

	this.scale = function(){
		return this.normalStdDev();
	};
}
HalfNormalDistribution.prototype = new FoldedNormalDistribution;

//Maxwell-Boltzmann distributiion with scale parameter b
function MaxwellBoltzmannDistribution(b0){
	var b, m, s, mx
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	m = 2 * b * Math.sqrt(2/Math.PI);
	s = b * Math.sqrt(3 - 8/Math.PI);
	mx = m + 4 * s;
	Distribution.call(this, 0, mx, mx/100, CONT);

	this.density = function(x){
		if (x >= 0) return Math.sqrt(2/Math.PI) * Math.pow(x, 2) * Math.exp(-Math.pow(x, 2)/(2 * Math.pow(b, 2)))/Math.pow(b, 3);
		else return 0;
	};

	this.mode = function(){
		return Math.sqrt(2) * b;
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return s * s;
	};

	this.simulate = function(){
		var r, theta, x, sum = 0;
		for (var i = 0; i < 3; i++){
			r = Math.sqrt(-2 * Math.log(Math.random()));
			theta = 2 * Math.PI * Math.random();
			sum = sum + Math.pow(b * r * Math.cos(theta), 2);
		}
		x = Math.sqrt(sum);
		this.setValue(x);
		return x;
	};

	this.scale = function(){
		return b;
	};
}
MaxwellBoltzmannDistribution.prototype = new Distribution;

//Student t distribution with n degrees of freedom
function StudentDistribution(n0){
	var n, mx, c;
	if (0 < n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	c = gamma((n + 1)/2)/(Math.sqrt(n * Math.PI) * gamma(n/2));
	if (n == 1) mx = 8;
	else if (n == 2) mx = 7;
	else mx = 4 * Math.sqrt(n/(n - 2));
	Distribution.call(this, -mx, mx, mx/50, CONT);

	this.mode = function(){
		return 0;
	};

	this.density = function(x){
		return c * Math.pow(1 + x * x/n, -(n + 1)/2);
	};

	this.CDF = function(x){
		var u = n/(n + x * x);
		if (x > 0) return 1 - 0.5 * betaCDF(u, 0.5 * n, 0.5);
		else return 0.5 * betaCDF(u, 0.5 * n, 0.5);
	};

	this.mean = function(){
		if (n == 1) return NaN;
		else return 0;
	};

	this.variance = function(){
		if (n == 1) return NaN;
		else if (n == 2) return Infinity;
		else return n/(n - 2);
	};

	this.simulate = function(){
		var x, v, z, r, theta;
		v = 0;
		for (var i = 1; i <= n; i++){
			r = Math.sqrt(-2 * Math.log(Math.random()));
			theta = 2 * Math.PI * Math.random();
			z = r * Math.cos(theta);
			v = v + z * z;
		}
		r = Math.sqrt(-2 * Math.log(Math.random()));
		theta = 2 * Math.PI * Math.random();
		z = r * Math.cos(theta);
		x = z/Math.sqrt(v/n);
		this.setValue(x);
		return x;
	};

	this.degrees = function(){
		return n;
	};
}
StudentDistribution.prototype = new Distribution;

//F distribution with n degrees of freedom in the numberator and d degrees of freedom in the denominator
function FDistribution(n0, d0){
	var n, d, mn, mx;
	if (0 < n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	if (0 < d0 && d0 < Infinity) d = Math.round(d0); else d = 1;
	var c = (gamma((n + d)/2)/(gamma(n/2) * gamma(d/2))) * Math.pow(n/d, n/2);
	if (n < 2) mn = 0.01; else mn = 0;
	if (d <= 4) mx = 20;
	else mx = d/(d - 2)  + 4 * Math.sqrt(2.0 * (d/(d - 2)) * (d/(d - 2)) * (d + n - 2)/(n * (d - 4)));
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	this.mode = function(){
		if (n <= 2) return mn;
		else return ((n - 2) * d)/(n * (d + 2));
	};

	this.density = function(x){
		return c * Math.pow(x, (n - 2)/2)/Math.pow(1 + (n/d) * x, (n + d)/2);

	};

	this.CDF = function(x){
		var u = d/(d + n * x);
		if (x <= 0) return 0;
		else return 1 - betaCDF(u, 0.5 * d, 0.5 * n);
	};

	this.mean = function(){
		if (d <= 2) return Infinity;
		else return d/(d - 2);
	};

	this.variance = function(){
		if (d <= 2) return NaN;
		else if (d <= 4) return Infinity;
		else return 2.0 * (d/(d - 2)) * (d/(d - 2))	* (d + n - 2)/(n * (d - 4));
	};

	this.simulate = function(){
		var x, u = 0, v = 0, z, r, theta;
		for (var i = 1; i <= n; i++){
			r = Math.sqrt(-2 * Math.log(Math.random()));
			theta = 2 * Math.PI * Math.random();
			z = r * Math.cos(theta);
			u = u + Math.pow(z, 2);
		}
		for (var j = 1; j <= d; j++){
			r = Math.sqrt(-2 * Math.log(Math.random()));
			theta = 2 * Math.PI * Math.random();
			z = r * Math.cos(theta);
			v = v + Math.pow(z, 2);
		}
		x = (u/n)/(v/d);
		this.setValue(x);
		return x;
	};

	this.numerator = function(){
		return n;
	};

	this.denominator = function(){
		return d;
	};
}
FDistribution.prototype = new Distribution;

//Lognormal distribution
function LogNormalDistribution(m0, s0){
	var m, s, mn, va, mx;
	if (-Infinity < m0 && m0 < Infinity) m = m0; else m = 0;
	if (0 < s0 && s0 < Infinity) s = s0; else s = 1;
	var mn = Math.exp(m + s * s/2);
	var va = Math.exp(2 * (m + s * s)) - Math.exp(2 * m + s * s);
	mx = mn + 3 * Math.sqrt(va);
	Distribution.call(this, 0, mx, mx/100, CONT);

	this.density = function(x){
		if (x === 0) return 0;
		else return Math.exp(-(Math.log(x) - m) * (Math.log(x) - m)/(2 * s * s))/(Math.sqrt(2 * Math.PI) * s * x);
	};

	this.mode = function(){
		return Math.exp(m - s * s);
	};

	this.CDF = function(x){
		var z;
		if (x <= 0) return 0;
		else{
			z = (Math.log(x) - m)/s;
			if (z >= 0) return 0.5 + 0.5 * gammaCDF(0.5 * z * z, 0.5);
			else return 0.5 - 0.5 * gammaCDF(0.5 * z * z, 0.5);
		}
	};

	this.mean = function(){
		return mn;
	};

	this.variance = function(){
		return va;
	};

	this.simulate = function(){
		var r = Math.sqrt(-2 * Math.log(Math.random()));
		var theta = 2 * Math.PI * Math.random();
		var x = Math.exp(m + s * r * Math.cos(theta));
		this.setValue(x);
		return x;
	};

	this.logScale = function(){
		return m;
	};

	this.shape = function(){
		return s;
	}
}
LogNormalDistribution.prototype = new Distribution;


//Distributions related to the Poisson and gamma

//Poisson distribution with rate r
function PoissonDistribution(r0){
	var r;
	if (0 <= r0 && r0 < Infinity) r = r0; else r = 1;
	Distribution.call(this, 0, Math.ceil(r + 4 * Math.sqrt(r)), 1, DISC);

	this.density = function(x){
		if (x < 0) return 0;
		else{
			var n = Math.round(x);
			return Math.exp(-r) * Math.pow(r, n)/factorial(n);
		};
	};

	this.mode = function(){
		return Math.floor(r);
	};

	this.CDF = function(x){
		return 1 - gammaCDF(r, x + 1);
	};

	this.mean = function(){
		return r;
	};

	this.variance = function(){
		return r;
	};

	this.simulate = function(){
		var arrivals = 0;
		var sum = -Math.log(1 - Math.random());
		while (sum <= r){
			arrivals++;
			sum = sum - Math.log(1 - Math.random());
		}
		this.setValue(arrivals);
		return arrivals;
	};

	this.rate = function(){
		return r;
	};
}
PoissonDistribution.prototype = new Distribution;

//Gamma Distribution with shape parameter k and scale parameter b
function GammaDistribution(k0, b0){
	var k, b, m, v, c, mn, mx;
	if (0 < k0 && k0 < Infinity) k = k0; else k = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	if (k >= 1) mn = 0; else mn = 0.01;
	m = k * b;
	v = k * Math.pow(b, 2);
	c = 1/(gamma(k) * Math.pow(b, k));
	mx = m + 4 * Math.sqrt(v);
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	this.mode = function(){
		if (k < 1) return mn;
		else return b * (k - 1);
	};

	this.density = function(x){
		if (x >= 0) return c * Math.pow(x, k - 1) * Math.exp(-x/b);
		else return 0;
	};

	this.CDF = function(x){
		return gammaCDF(x/b, k);
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return v;
	};

	this.shape = function(){
		return k;
	}

	this.scale = function(){
		return b;
	}

	this.rate = function(){
		return 1/b;
	};
}
GammaDistribution.prototype = new Distribution;

//Chi-square distribution with n degrees of freedom
function ChiSquareDistribution(n0){
	var n;
	if (0 < n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	GammaDistribution.call(this, n/2, 2);

	this.simulate = function(){
		var v = 0, z, r, theta;
		for (var i = 1; i <= n; i++){
			r = Math.sqrt(-2 * Math.log(Math.random()));
			theta = 2 * Math.PI * Math.random();
			z = r * Math.cos(theta);
			v = v + Math.pow(z, 2);
		}
		this.setValue(v);
		return v;
	};

	this.degrees = function(){
		return n;
	};
}
ChiSquareDistribution.prototype = new GammaDistribution;

//Exponential distribution with scale parameter b
function ExponentialDistribution(b0){
	GammaDistribution.call(this, 1, b0);
	var b = this.scale();

	this.CDF = function(x){
		return 1 - Math.exp(-x/b);
	}

	this.quantile = function(q){
		return -b * Math.log(1 - q);
	}

}
ExponentialDistribution.prototype = new GammaDistribution;

//Distribution related to the beta distribution

//Beta distribution with left shape parameter a and right shape parameter b
function BetaDistribution(a0, b0){
	var a, b, mn, mx;
	if (0 < a0 && a0 < Infinity) a = a0; else a = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	var c = gamma(a + b)/(gamma(a) * gamma(b));
	if (a < 1) mn = 0.01; else mn = 0;
	if (b < 1) mx = 0.99; else mx = 1;
	Distribution.call(this, mn, mx, 0.01, CONT);

	this.mode = function(){
		var m;
		if (a < 1 && b < 1){
			if (a < b) m = 0.01; else m = 0.99;
		}
		else if (a < 1 && b >= 1) m = 0.01;
		else if (a >= 1 && b < 1) m = 0.99;
		else if (a >= 1 && b == 1) m = 1;
		else if (a == 1 && b >= 1) m = 0;
		else m = (a - 1)/(a + b - 2);
		return m;
	};

	this.density = function(x){
		if (0 <= x && x <= 1) return c * Math.pow(x, a - 1) * Math.pow(1 - x, b - 1);
		else return 0;
	};

	this.CDF = function(x){
		if (x <= 0) return 0;
		else if (x >= 1) return 1;
		else return betaCDF(x, a, b);
	};

	this.mean = function(){
		return a/(a + b);
	};

	this.variance = function(){
		return a * b/((a + b) * (a + b) * (a + b + 1));
	};

	this.left = function(){
		return a;
	};

	this.right = function(){
		return b;
	};
}
BetaDistribution.prototype = new Distribution;

//Beta prime distribution with shape parameters a and b
function BetaPrimeDistribution(a0, b0){
	var a, b, c, mn, mx;
	if (0 < a0 && a0 < Infinity) a = a0; else a = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	c = gamma(a + b)/(gamma(a) * gamma(b));
	if (a >= 1) mn = 0;	else mn = 0.01;
	mx = 4 * a/b;
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	this.mode = function(){
		if (a >=1) return (a - 1)/(b + 1);
		else return this.minValue();
	};

	this.density = function(x){
		if (x > 0 || (x == 0) && (a == 1)) return c * Math.pow(x, a - 1) * Math.pow(1 + x, -a - b);
		else return 0;
	};

	this.CDF = function(x){
		if (x <= 0) return 0; else return betaCDF(x/(x + 1), a, b);
	};

	this.mean = function(){
		if (b > 1) return a/(b - 1);
		else return Infinity;
	}

	this.variance = function(){
		if (b > 2) return a * (a + b - 1)/((b - 2) * Math.pow(b - 1, 2)) ;
		else if (b > 1) return Infinity;
		else return NaN;
	}

	this.left = function(){
		return a;
	};

	this.right = function(){
		return b;
	};
}
BetaPrimeDistribution.prototype = new Distribution;

//Uniform  and related distributions

//Uniform distribution on the interval [a, a + w]
function UniformDistribution(a0, w0){
	var a, w;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < w0 & w0 < Infinity) w = w0; else w = 1;
	Distribution.call(this, a, a + w, w/100, CONT);

	this.density = function(x){
		if (a <= x && x <= a + w) return 1/w;
		else return 0;
	};

	this.mode = function(){
		return a;
	};

	this.CDF = function(x){
		if (x < a) return 0;
		else if (x > a + w) return 1;
		else return (x - a)/w;
	};

	this.quantile = function(p){
		if (p < 0 || p > 1) return NaN;
		else return a + p * w;
	};

	this.mean = function(){
		return a + w/2;
	};

	this.variance = function(){
		return Math.pow(w, 2)/12;
	};

	this.location = function(){
		return a;
	}

	this.scale = function(){
		return w;
	}
}
UniformDistribution.prototype = new Distribution;

//Discrete uniform distribution on the points {a, a + h, ..., a + (n - 1) h}
function DiscreteUniformDistribution(a0, n0, h0){
	var a, n, h, b;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	if (0 < h0 && h0 < Infinity) h = h0; else h = 1;
	b = a + (n - 1) * h;
	Distribution.call(this, a, b, h, DISC);

	this.density = function(x){
		var j = Math.round((x - a) / h);
		if (0 <= j && j < n) return 1/n;
		else return 0;
	};

	this.mode = function(){
		return a;
	};

	this.CDF = function(x){
		var j = Math.round((x - a) / h);
		if (j < 0) return 0;
		else if (j > n - 1) return 1;
		else return (j + 1)/n;
	};

	this.simulate = function(){
		var x = a + h * Math.floor(n * Math.random());
		this.setValue(x);
		return x;
	};

	this.mean = function(){
		return (a + b)/2;
	};

	this.variance = function(){
		return (n - 1)*(n + 1) * h * h / 12;
	};

	this.left = function(){
		return a;
	};

	this.points = function(){
		return n;
	};
}
DiscreteUniformDistribution.prototype = new Distribution;

//Irwin-Hall distribution
function IrwinHallDistribution(n0){
	var n;
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	Distribution.call(this, 0, n, n/100, CONT);

	this.mode = function(){
		return n/2;
	};

	this.density = function(x){
		var sum = 0;
		if (n == 1) return 1;
		else {
			for (var k = 0; k <= n; k++) sum = sum + Math.pow(-1, k) * binomial(n, k)*  Math.pow(x - k, n - 1) * sgn(x - k);
			return sum/(2 * factorial(n - 1));
		}
	};

	this.CDF = function(x){
		var sum = 0;
		if (x < 0) return 0;
		else if (x > n) return 1;
		else{
			for (var k = 0; k <= n; k++) sum = sum + Math.pow(-1, k) * binomial(n, k) * sgn(x - k) * Math.pow(x - k, n);
			return 0.5 + sum/(2 * factorial(n));
		}
	};

	this.simulate = function(){
		var sum = 0;
		for (var i = 0; i < n; i++) sum = sum + Math.random();
		this.setValue(sum);
		return sum;
	};

	this.mean = function(){
		return n/2;
	};

	this.variance = function(){
		return n/12;
	};

	this.order = function(){
		return n;
	};

}
IrwinHallDistribution.prototype = new Distribution;

//Distributions based on shapes

//Triangle distribution on the interval [a, a + w] with vertex at a + p w
function TriangleDistribution(a0, w0, p0){
	var a, w, p;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < w0 && w0 < Infinity) w = w0; else w = 1;
	if (0 <= p0 && p0 <= 1) p = p0; else p = 0.5;
	Distribution.call(this, a, a + w, w/100, CONT);

	this.standardDensity = function(x){
		if (p == 0 && 0 <= x && x <= 1) return 2*(1 - x);
		else if (p == 1 && 0 <= x && x <= 1) return 2*x;
		else if (0 <= x && x <= p) return 2*x/p;
		else if (p < x && x <= 1) return 2*(1 - x)/(1 - p);
		else return 0;
	};

	this.standardCDF = function(x){
		if (x < 0) return 0;
		else if (p == 0 && 0 <= x && x <= 1) return 1 - Math.pow(1 - x, 2);
		else if (p == 1 && 0 <= x && x <= 1) return Math.pow(x, 2);
		else if (0 <= x && x <= p) return Math.pow(x, 2)/p;
		else if (p < x && x <= 1) return 1 - Math.pow(1 - x, 2)/(1 - p);
		else return 1;
	};

	this.standardQuantile = function(q){
		if (0 <= q && q <= p) return Math.sqrt(q*p);
		else if (p < q && q <= 1) return 1 - Math.sqrt((1 - q)*(1 - p));
		else return NaN;
	}

	this.density = function(x){
		return (1/w) * this.standardDensity((x - a)/w);
	};

	this.CDF = function(x){
		return this.standardCDF((x - a)/w);
	};

	this.quantile = function(q){
		return a + w * this.standardQuantile(q);
	};

	this.mode = function(){
		var m = a + p * w;
		if (p == 1) m = a + w;
		return m;
	};

	this.mean = function(){
		return a + w * (1 + p)/3;
	};

	this.variance = function(){
		return Math.pow(w, 2)*(1 - p + Math.pow(p, 2))/18;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return w;
	};

	this.shape = function(){
		return p;
	};
}
TriangleDistribution.prototype = new Distribution;

//Semicircle distribution with center a and radius r
function SemiCircleDistribution(a0, r0){
	var a, r;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < r0 && r0 < Infinity) r = r0; else r = 1;
	Distribution.call(this, a - r, a + r, r/50, CONT);

	this.standardDensity = function(x){
		if (x < -1 || x > 1) return 0;
		else return 2 * Math.sqrt(1 - x*x)/Math.PI;
	};

	this.standardCDF = function(x){
		if (x < -1) return 0;
		else if (x > 1) return 1;
		else return 0.5 + x*Math.sqrt(1 - x*x)/Math.PI + Math.asin(x)/Math.PI;
	};

	this.density = function(x){
		return (1/r)*this.standardDensity((x - a)/r);
	};

	this.CDF = function(x){
		return this.standardCDF((x - a)/r);
	};

	this.mode = function(){
		return a;
	};

	this.mean = function(){
		return a;
	};

	this.variance = function(){
		return r*r/4;
	};

	this.simulate = function(){
		var u, v, t, x;
		u = Math.random();
		v = Math.random();
		t = Math.max(u, v);
		theta = 2*Math.PI*Math.random();
		x = a + r*t*Math.cos(theta);
		this.setValue(x);
		return x;
	};

	this.center = function(){
		return a;
	}

	this.radius = function(){
		return r;
	};

}
SemiCircleDistribution.prototype = new Distribution;

//U-quadratic distribution with location parameter a0 and scale parameter w0
function UPowerDistribution(k0, mu0, c0){
	var k, mu, ca;
	if (0 < k0 && k0 < Infinity) k = Math.round(k0); else k = 1;
	if (-Infinity < mu0 && mu0 < Infinity) mu = mu0; else mu = 0;
	if (0 < c0 && c0 < Infinity) c = c0; else c = 1;
	Distribution.call(this, mu - c, mu + c, c/50, CONT);

	this.density = function(x){
		if (mu - c <= x && x <= mu + c) return ((2*k + 1)/(2*c))*Math.pow((x - mu)/c, 2*k);
		else return 0;
	};

	this.CDF = function(x){
		if (x < mu - c) return 0;
		else if (x > mu + c) return 1;
		else return 0.5*(1 + Math.pow((x - mu)/c, 2*k + 1));
	};

	this.quantile = function(p){
		if (0 <= p && p <= 0.5) return mu - c * Math.pow(1 - 2*p, 1/(2*k + 1));
		else if (0.5 < p && p <= 1) return mu + c * Math.pow(2*p - 1, 1/(2*k +1));
		else return NaN;
	}

	this.mode = function(){
		return mu - c;
	};

	this.mean = function(){
		return mu;
	};

	this.variance = function(){
		return Math.pow(c, 2)*(2*k + 1)/(2*k + 3);
	};

	this.shape = function(){
		return k;
	};

	this.location = function(){
		return mu;
	};

	this.scale = function(){
		return c;
	};
}
UPowerDistribution.prototype = new Distribution;

//Other continuous distributions

//Weibull distribution with shape parameter k and scale parameter b
function WeibullDistribution(k0, b0){
	var k, b, c, m, v, mx;
	if (0 < k0 && k0 < Infinity) k = k0; else k = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	c = k/Math.pow(b, k);
	m = b * gamma(1 + 1/k);
	v = b * b * gamma(1 + 2/k) - Math.pow(m, 2);
	mx = m + 4 * Math.sqrt(v);
	Distribution.call(this, 0, mx, mx/100, CONT);

	this.density = function(x){
		if (x >= 0) return c * Math.pow(x, k - 1) * Math.exp(-Math.pow(x/b, k));
		else return 0;
	};

	this.mode = function(){
		if (k < 1) return this.minValue;
		else return b * Math.pow((k - 1)/k, 1/k);
	};

	this.CDF = function(x){
		return 1 - Math.exp(-Math.pow(x/b, k));
	};

	this.quantile = function(p){
		return b * Math.pow(-Math.log(1 - p), 1/k);
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return v;
	};

	this.shape = function(){
		return k;
	}

	this.scale = function(){
		return b;
	}
}
WeibullDistribution.prototype = new Distribution;

//Pareto distribution with shape parameter k and scale parameter b
function ParetoDistribution(k0, b0){
	var k, b, mx;
	if (0 < k0 && k0 < Infinity) k = k0; else k = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	c = k * Math.pow(b, k);
	mx = b * (1 + 6/k);
	Distribution.call(this, b, mx, (mx - b)/100, CONT);

	this.density = function(x){
		if (x < b) return 0;
		else return c/Math.pow(x, k + 1);
	};

	this.mode = function(){
		return b;
	};

	this.CDF = function(x){
		return 1 - Math.pow(b/x, k);
	};

	this.quantile = function(p){
		return b/Math.pow((1 - p), 1/k);
	};

	this.mean = function(){
		if (k <= 1) return Infinity;
		else return (k * b)/(k - 1);
	};

	this.variance = function(){
		if (k <= 1) return NaN;
		else if (k > 1 && k <= 2) return Infinity;
		else return (k * b * b)/((k - 1) * (k - 2) * (k - 2));
	};

	this.shape = function(){
		return k;
	};

	this.scale = function(){
		return b;
	};
}
ParetoDistribution.prototype = new Distribution;

//Logistic distribution with location parameter a and scale parameter b
function LogisticDistribution(a0, b0){
	var a, b, m, v, mn, mx;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	m = a;
	v = (b * b * Math.PI * Math.PI)/3;
	mn = m - 4 * Math.sqrt(v);
	mx = m + 4 * Math.sqrt(v);
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	this.density = function(x){
		var e = Math.exp((x - a)/b);
		return e/(b * (1 + e) * (1 + e));
	};

	this.mode = function(){
		return a;
	};

	this.CDF = function(x){
		var e = Math.exp((x - a)/b);
		return e/(1 + e);
	};

	this.quantile = function(p){
		return a + b * Math.log(p/(1 - p));
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return v;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return b;
	};
}
LogisticDistribution.prototype = new Distribution;

//Log-logistic distribution with scale parameter a and shape parameter b
function LogLogisticDistribution(a0, b0){
	var a, b, mn, mx;
	if (0 < a0 && a0 < Infinity) a = a0; else a = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	if (b >= 1) mn = 0; else mn = 0.001;
	if (b >= 2) mx = a * Math.pow(100, 1/b); else mx = a * 10;
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	this.density = function(x){
		if (x >= 0) return (b/a) * Math.pow(x/a, b - 1)/Math.pow(1 + Math.pow(x/a, b), 2);
		else return 0;
	};

	this.mode = function(){
		if (b >= 1) return a * Math.pow((b - 1)/(b + 1), 1/b);
		else return this.minValue();
	};

	this.mean = function(){
		if (b > 1) return ((a * Math.PI)/b)/Math.sin(Math.PI/b);
		else return Infinity;
	};

	this.variance = function(){
		var t = Math.PI/b;
		if (b > 2) return Math.pow(a, 2) * ((2 * t)/Math.sin(2 * t) - Math.pow(t, 2)/Math.pow(Math.sin(t), 2));
		else if (b > 1) return Infinity;
		else return NaN;
	};

	this.CDF = function(x){
		if (x < 0) return 0;
		else return Math.pow(x, b)/(Math.pow(a, b) + Math.pow(x, b));
	};

	this.quantile = function(p){
		if (0 <= p && p <= 1) return a * Math.pow(p/(1 - p), 1/b);
		else return NaN;
	};

	this.scale = function(){
		return a;
	};

	this.shape = function(){
		return b;
	};
}
LogLogisticDistribution.prototype = new Distribution;

//Extreme value distribution with location paramter a and scale parameter b
function ExtremeValueDistribution(a0, b0){
	var a, b, m, v, mn, mx;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	m = a + b * EULER; v = Math.pow(b * Math.PI, 2)/6;
	mx = m + 4 * Math.sqrt(v); mn = m - 4 * Math.sqrt(v);
	Distribution.call(this, mn, mx, (mx - mn)/100, CONT);

	this.density = function(x){
		var e = Math.exp(-(x - a)/b);
		return e * Math.exp(-e)/b;
	};

	this.mode = function(){
		return a;
	};

	this.CDF = function(x){
		return Math.exp(-Math.exp(-(x - a)/b));
	};

	this.quantile = function(p){
		return a - b * Math.log(-Math.log(p));
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return v;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return b;
	};
}
ExtremeValueDistribution.prototype = new Distribution;

//Exponential-Logarithmic distribution with shape parameter p and scale parameter b
function ExponentialLogarithmicDistribution(p0, b0){
	var p, b;
	if (0 < p0 && p0 < 1) p = p0; else p = 0.5;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	Distribution.call(this, 0, 4/b, 1/(25*b), CONT);

	this.mode = function(){
		return 0;
	};

	this.density = function(x){
		if (x >= 0) return -b * (1 - p) * Math.exp(-b * x)/(Math.log(p) * (1 - (1 - p) * Math.exp(-b * x)));
		else return 0;
	};

	this.CDF = function(x){
		if (x < 0) return 0;
		else return 1 - Math.log(1 - (1 - p) * Math.exp(-b * x))/Math.log(p);
	};

	this.quantile = function(q){
		if (0 <= q && q <= 1) return Math.log((1 - p)/(1 - Math.pow(p, 1 - q)))/b;
		else return NaN;
	};

	this.mean = function(){
		return -polyLog(2, 1 - p)/(b * Math.log(p));
	}

	this.variance = function(){
		return -2 * polyLog(3, 1 - p)/(Math.pow(b, 2) * Math.log(p)) - Math.pow(this.mean(), 2);
	}
}
ExponentialLogarithmicDistribution.prototype = new Distribution;

//Cauchy distribution with location parameter a and scale parameter b
function CauchyDistribution(a0, b0){
	var a, b;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	Distribution.call(this, a - 5 * b, a + 5 * b, b/10, CONT);

	this.mode = function(){
		return a;
	};

	this.density = function(x){
		var u = x - a
		return b/(Math.PI * ( b * b + u * u));
	};

	this.CDF = function(x){
		return 0.5 + (1/Math.PI) * Math.atan((x - a)/b);
	};

	this.quantile = function(p){
		return a + b * Math.tan(Math.PI * (p - 0.5));
	};

	this.mean = function(){
		return NaN;
	};

	this.variance = function(){
		return NaN;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return b;
	};

}
CauchyDistribution.prototype = new Distribution;

//Arcsine distribution with location parameter a and scale parameter w
function ArcsineDistribution(a0, w0){
	var a, w, d;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < w0 && w0 < Infinity) w = w0; else w = 1;
	d = w/100;
	Distribution.call(this, a + d, a + w - d, d, CONT);

	this.mode = function(){
		return NaN;
	};

	this.maxDensity = function(){
		return this.density(this.minValue());
	};

	this.density = function(x){
		return 1/(Math.PI * Math.sqrt((x - a) * (a + w - x)));
	};

	this.CDF = function(x){
		return (2/Math.PI) * Math.asin(Math.sqrt((x - a)/w));
	};

	this.quantile = function(p){
		return a + w * Math.pow(Math.sin(p * Math.PI/2), 2);
	};

	this.mean = function(){
		return a +  w/2;
	};

	this.variance = function(){
		return Math.pow(w, 2)/8;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return w;
	};

}
ArcsineDistribution.prototype = new Distribution;

//Hyperbolic Secant distribution
function HyperbolicSecantDistribution(m0, s0){
	var m, s;
	if (-Infinity < m0 && m0 < Infinity) m = m0; else m = 0;
	if (0 < s0 && s0 < Infinity) s = s0; else s = 1;
	Distribution.call(this, m - 4 * s, m + 4 * s, (4 * s)/25, CONT);

	this.mode = function(){
		return m;
	};

	this.density = function(x){
		var t = (Math.PI/2) * ((x - m)/s);
		return 1/(s * (Math.exp(t) + Math.exp(-t)));
	};

	this.CDF = function(x){
		return (2/Math.PI) * Math.atan(Math.exp((Math.PI/2) * (x - m)/s));
	};

	this.quantile = function(p){
		return m + s * (2/Math.PI) * Math.log(Math.tan((Math.PI/2) * p));
	};

	this.mean = function(){
		return m;
	};

	this.stdDev = function(){
		return s;
	};

	this.variance = function(){
		return Math.pow(s, 2);
	};

	this.location = function(){
		return m;
	};

	this.scale = function(){
		return s;
	};
}
HyperbolicSecantDistribution.prototype = new Distribution;

//Laplace distribution with location parameter a and scale parameter b
function LaplaceDistribution(a0, b0){
	var a, b;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	Distribution.call(this, a - 5 * b, a + 5 * b, b/10, CONT);

	this.mode = function(){
		return a;
	};

	this.density = function(x){
		return Math.exp(-Math.abs(x - a)/b)/(2 * b);
	};

	this.mean = function(){
		return a;
	};

	this.variance = function(){
		return 2 * Math.pow(b, 2);
	};

	this.quantile = function(p){
		if (p <= 0.5) return a + b * Math.log(2 * p);
		else return a - b * Math.log(2 * (1 - p));
	};

	this.CDF = function(x){
		if (x <= a) return 0.5 * Math.exp((x - a)/b);
		else return 1 - 0.5 * Math.exp(-(x - a)/b);
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return b;
	};
}
LaplaceDistribution.prototype = new Distribution;

//Rayleigh distribution with scale parameter b
function RayleighDistribution(b0){
	var b, m, s, mx;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;

	m = b * Math.sqrt(Math.PI/2);
	s = b * Math.sqrt(2 - Math.PI/2);
	mx = m + 4 * s;
	Distribution.call(this, 0, mx, mx / 100, CONT);

	this.density = function(x){
		if (x >= 0) return (x / Math.pow(b, 2)) * Math.exp(-0.5 * Math.pow(x / b, 2));
		else return 0;
	};

	this.CDF = function(x){
		if (x >= 0) return 1 - Math.exp(-0.5 * Math.pow(x / b, 2));
		else return 0;
	};

	this.quantile = function(p){
		if (p >= 0 && p < 1) return b * Math.sqrt(-2 * Math.log(1 - p));
		else if (p == 1) return Infinity;
		else return NaN;
	};

	this.mode = function(){
		return b;
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return Math.pow(s, 2);
	};

	this.scale = function(){
		return b;
	};
}
RayleighDistribution.prototype = new Distribution;

//Levy distribution with location parameter a and scale parameter b
function LevyDistribution(a0, b0){
	var a, b, mx;
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	mx = a + 6 * b;
	Distribution.call(this, a, mx, (mx - a)/100, CONT);

	this.density = function(x){
		if (x > a) return Math.sqrt(b/(2*Math.PI)) * (1/Math.pow(x - a, 3/2)) * Math.exp(-b/(2 * (x - a)));
		else return 0;
	};

	this.CDF = function(x){
		if (x >= a) return 1 - erf(Math.sqrt(b / (2 * (x - a))));
		else return 0;
	};

	this.mode = function(){
		return a + b/3;
	};

	this.simulate = function(){
		var r = Math.sqrt(-2 * Math.log(Math.random()));
		var theta = 2 * Math.PI * Math.random();
		var z = r * Math.cos(theta);
		var x = a + b/Math.pow(z,2);
		this.setValue(x);
		return x;
	};

	this.mean = function(){
		return Infinity;
	};

	this.variance = function(){
		return NaN;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return b;
	};
}
LevyDistribution.prototype = new Distribution;

//Gompertz distribution with shape parameter k and scale parameter b
function GompertzDistribution(k0, b0){
	var k, b, c, md, iqr, mx;
	if (0 < k0 && k0 < Infinity) k = k0; else k = 1;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1;
	c = (k / b) * Math.exp(k);
	mx = b * Math.log(1 - (1 / k) * Math.log(0.0005));
	Distribution.call(this, 0, mx, mx/100, CONT);

	this.density = function(x){
		var e;
		if (x < 0) return 0;
		else {
			e = Math.exp(x / b);
			return c * e * Math.exp(-k * e);
		}
	};

	this.mode = function(){
		if (k < 1) return -b * Math.log(k);
		else return 0;
	};

	this.CDF = function(x){
		if (x < 0) return 0;
		else return 1 - Math.exp(-k * (Math.exp(x / b) - 1));
	};

	this.quantile = function(p){
		return b * Math.log(1 - (1 / k) * Math.log(1 - p));
	};

	this.shape = function(){
		return k;
	};

	this.scale = function(){
		return b;
	};
}
GompertzDistribution.prototype = new Distribution;



//Distributions related to Benford's law

//Benford mantissa distribution with base b
function BenfordMantissaDistribution(b0){
	var b;
	if (1 < b0 && b0 < Infinity) b = b0; else b = 10;
	Distribution.call(this, 1/b, 1, (1 - 1/b)/100, CONT);

	this.mode = function(){
		return this.minValue();
	};

	this.density = function(x){
		return 1/(x * Math.log(b));
	};

	this.CDF = function(x){
		return 1 + Math.log(x)/Math.log(b);
	};

	this.quantile = function(p){
		return 1/Math.pow(b, 1 - p);
	};

	this.mean = function(){
		return (b - 1)/(b * Math.log(b));
	};

	this.variance = function(){
		return ((b - 1)/(Math.pow(b, 2) * Math.log(b))) * ((b + 1)/2 - (b - 1)/Math.log(b));
	};

	this.base = function(){
		return b;
	};
}
BenfordMantissaDistribution.prototype = new Distribution;

//Benford first digit distribution with base b
function BenfordDigitDistribution(b0){
	var b;
	if (1 < b0 && b0 < Infinity) b = b0; else b = 10;
	Distribution.call(this, 1, b - 1, 1, DISC);

	this.mode = function(){
		return 1;
	};

	this.density = function(x){
		return (Math.log(x + 1) - Math.log(x))/Math.log(b);
	};

	this.CDF = function(x){
		return Math.log(x + 1)/Math.log(b);
	}

	this.quantile = function(p){
		return Math.ceil(Math.pow(b, p) - 1);
	}

	this.base = function(){
		return b;
	};
}
BenfordDigitDistribution.prototype = new Distribution;

//Other discrete distributions

//Zeta distribution with shape parameter a
function ZetaDistribution(a0){
	var a, c;
	if (1 < a0 && a0 < Infinity) a = a0; else a = 2;
	Distribution.call(this, 1, Math.ceil(Math.pow(10, 3.5/a)), 1, DISC);
	c = zeta(a);

	this.density = function(x){
		return 1/(c * Math.pow(x, a));
	};


	this.mean = function(){
		if (a > 2) return zeta(a - 1)/zeta(a);
		else return Infinity;
	};

	this.variance = function(){
		if (a > 3) return zeta(a - 2)/zeta(a) - this.mean() * this.mean();
		else if (a > 2) return Infinity;
		else return NaN;
	};

	this.shape = function(){
		return a;
	};
}
ZetaDistribution.prototype = new Distribution;

//Logarithmic series distribution with shape parameter p
function LogarithmicDistribution(p0){
	var p, m, s2;
	if (0 < p0 && p0 < 1) p = p0; else p = 0.5;

	m = -p/(Math.log(1 - p) * (1 - p));
	s2 = -p * (p + Math.log(1 - p))/(Math.pow(1 - p, 2) * Math.pow(Math.log(1 - p), 2));
	Distribution.call(this, 1, m + 4 * Math.sqrt(s2), 1, DISC);

	this.density = function(x){
		return -Math.pow(p, x)/(x * Math.log(1 - p));
	};

	this.mode = function(){
		return 1;
	};

	this.mean = function(){
		return m;
	};

	this.variance = function(){
		return s2;
	};

	this.shape = function(){
		return p;
	};
}
LogarithmicDistribution.prototype = new Distribution;

//General classes of distributions

//The location scale distribution with distribution d, location parameter a, and scale parameter b
function LocationScaleDistribution(d0, a0, b0){
	var d, a, b;
	if (d0 instanceof Distribution) d = d0; else d = new NormalDistribution(0, 1);
	if (-Infinity < a0 && a0 < Infinity) a = a0; else a = 0;
	if (0 < b0 && b0 < Infinity) b = b0; else b = 1
	Distribution.call(this, a + b * d.minValue(), a + b * d.maxValue(), b * d.step(), d.type());

	this.density = function(x){
		var y = (x - a)/b;
		if (this.type() === DISC) return d.density(y);
		else return d.density(y)/b;
	};

	this.mode = function(){
		return a + b * d.mode;
	};

	this.maxDensity = function(){
		if (this.type() === DISC) return d.maxDensity();
		else return d.maxDensity()/b;
	};

	this.CDF = function(x){
		return d.CDF((x - a)/b);
	};

	this.quantile = function(p){
		return a + b * d.quantile(p);
	};

	this.mean = function(){
		return a + b * d.mean();
	};

	this.variance = function(){
		return b * b * d.variance();
	};

	this.simulate = function(){
		var x = a + b * d.simulate();
		this.setValue(x);
		return x;
	};

	this.dist = function(){
		return d;
	};

	this.location = function(){
		return a;
	};

	this.scale = function(){
		return b;
	};

	this.type = function() {
		return d.type();
	};
}
LocationScaleDistribution.prototype = new Distribution;

//Convolution power of a distribution d of order n
function Convolution(d0, n0){
	var d, n;
	if (d0 instanceof Distribution) d = d0; else d = new UniformDistribution(0, 1);
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	Distribution.call(this, n * d.minValue(), n * d.maxValue(), d.step(), d.type());

	var a = d.minValue(), b = d.maxValue(), s = d.step();
	var pdf = new Array(n);

	var m = Math.round((b - a)/s) + 1;
	var delta = 1;

	if (this.type() == CONT) delta = this.step();

	for (var k = 0; k < n; k++) pdf[k] = new Array((k + 1) * m - k);
	for (var j = 0; j < m; j++) pdf[0][j] = d.density(a + j * s);
	for (var k1 = 1; k1 < n; k1++){
		for (var j1 = 0; j1 < (k1 + 1) * m - k1; j1++){
			var sum = 0;
			for (var i = Math.max(0, j1 - m + 1); i < Math.min(j1 + 1, k1 * m - k1 + 1); i++)	sum = sum + pdf[k1 - 1][i] * pdf[0][j1 - i] * delta;
			pdf[k1][j1] = sum;
		}
	}

	this.density = function(x){
		var index = Math.round((x - this.minValue())/this.step());
		return pdf[n - 1][index];
	};

	this.mean = function(){
		return n * d.mean();
	};

	this.variance = function(){
		return n * d.variance();
	};

	this.simulate = function(){
		var sum = 0;
		for (i = 1; i <= n; i++) sum = sum + d.simulate();
		this.setValue(sum);
		return sum;
	};

	this.dist = function(){
		return d;
	};

	this.power = function(){
		return n;
	};
}
Convolution.prototype = new Distribution;

//Distribution of an order statistic from distribution d with sample size n and order k
function OrderStatistic(d0, n0, k0){
	var d, n, k;
	if (d0 instanceof Distribution) d = d0; else d = new UniformDistribution(0, 1);
	if (1 <= n0 && n0 < Infinity) n = Math.round(n0); else n = 1;
	if (1 <= k0 && k0 <= n) k = Math.round(k0); else k = 1;
	Distribution.call(this, d.minValue(), d.maxValue(), d.step(), d.type());

	this.density =  function(x){
		if (this.type() === DISC) return this.CDF(x) - this.CDF(x - this.step());
		else {
			var p = d.CDF(x);
			return k * binomial(n, k) * Math.pow(p, k - 1) * Math.pow(1 - p, n - k) * d.density(x);
		}
	};

	this.CDF = function(x){
		var sum = 0, p = d.CDF(x);
		for (var j = k; j <= n; j++) sum = sum + binomial(n, j) * Math.pow(p, j) * Math.pow(1 - p, n - j);
		return sum;
	};

	this.simulate = function(){
		var sampleValues = new Array(n);
		var orderStats = new Array(n);
		for (var i = 0; i < n; i++) sampleValues[i] = d.simulate();
		orderStats = sampleValues.sort(ascend);
		var x = orderStats[order - 1];
		this.setValue(x);
		return x;
	};

	this.dist = function(){
		return d;
	};

	this.sample = function(){
		return n;
	};

	this.order = function(){
		return k;
	};
}
OrderStatistic.prototype = new Distribution;

//Addition from R: square of random variable
function PowerDistribution(d0, n0){
	var d, n;
	if (d0 instanceof Distribution) d = d0; else d = new UniformDistribution(0, 1);
	if (0 < n0 && n0 < Infinity) n = n0; else n = 2;
	if (d.type() === CONT ) {
		var m1,m2;
		if (d.minValue() < 0) m1 = 0; else m1 = Math.pow(d.minValue(),n);
		m2 = Math.pow(d.maxValue(),n);
		Distribution.call(this, m1 , m2, d.step(), d.type());
	} else {
		var values,pdf=[];
		//console.log(["orig",d.values()]);
		values=d.values().map(function(x) {return Opal.CqlsAEP.$quantize(Math.pow(x,n));}).sort(ascend);
		//console.log(["values:",values]);
		for(var i=0; i<values.length;i++) pdf[i]=0;
		var tmp=d.values();
		for(i=0; i<tmp.length;i++) pdf[values.indexOf(Opal.CqlsAEP.$quantize(Math.pow(tmp[i],n)))]+=d.density(tmp[i]);
		//console.log(["power",values,pdf]);
		Distribution.call(this, values[0] , values[values.length-1], -1, d.type());
	}
	//console.log("values:"+[d.minValue(),d.maxValue(),d.step()]+":"+d.values());

	//only for discrete rv
	if (this.type() === DISC) {
		this.index = function(x) {
			return this.values().indexOf(Opal.CqlsAEP.$quantize(x));
		}

		this.isValue = function(x) {
			//console.log(["power is value",x,this.values(),this.values().indexOf(x),this.values().indexOf(x) >= 0])
			return this.index(x) >= 0;
		}

		this.regular = function() {
			return false;
		}
	}

	this.values = function() {
		return values;
	}

	this.density = function(x){
		var delta=0;
		if (this.type() === DISC) {
			return (this.isValue(x) ? pdf[this.index(x)] : 0);
		} else {
			if(x==0) return 0.0; //otherwise it fails!
			delta = 1.0/n * Math.pow(x,-1/n);
			return  d.density(-Math.pow(x,1/n))*delta + d.density(Math.pow(x,1/n))*delta;
		}
		//console.log( [x,delta,delta2,d.density(-Math.pow(x,1/n))*delta , d.density(Math.pow(x,1/n))*delta2] );

	};

	this.mean = function(){
		return d.rawMoment(n);
	};

	this.variance = function(){
		return d.rawMoment(2*n) - Math.pow(this.mean(),2);
	};

	this.simulate = function(){
		return Math.pow(d.simulate(),n);
	};

	this.dist = function(){
		return d;
	};

	this.power = function(){
		return n;
	};
}
PowerDistribution.prototype = new Distribution;

function Convolution2(d1, d2, b1, b2) {
	//console.log("conv2");console.log([d1.density(0),d2.density(0),b1,b2])
	var conv=Opal.CqlsAEP.Convolution.$new(d1,d2,b1,b2);
	//console.log(conv);
	Distribution.call(this, d1.minValue()+d2.minValue(), d1.maxValue()+d2.maxValue(), -1 , DISC);

	this.regular = function() {
		return false;
	}

	this.isValue = function(x) {
		return this.values().indexOf(x) >= 0;
	}

	this.values = function() {
		return conv.bounds;
	}

	this.density = function(x){
		var index = conv.bounds.indexOf(x);
		return index < 0 ? 0 : conv.pdf[index];
	};

	this.mean = function(){
		return d1.mean() + d2.mean();
	};

	this.variance = function(){
		return d1.variance() + d2.variance();
	};

	this.simulate = function(){
		var sum = d1.simulate() + d2.simulate();
		this.setValue(sum);
		return sum;
	};

}
Convolution2.prototype = new Distribution;
