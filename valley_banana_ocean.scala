object LetsGetMoving {
  //Create a simple moving average
  def simpleMovingAverage(values: Seq[Double]): Seq[Double] = {
    val sum = values.sum
    val size = values.size
    val average = if (size > 0) sum / size else 0
    Seq.fill(size)(average)
  }

  //Create an exponentially weighted moving average 
  def expMovingAverage(values: Seq[Double], 
                            decay: Double = 0.99): Seq[Double] = {
    var sum = 0.0
    def emit(v: Double): Seq[Double] = {
      sum = sum * decay + v
      Seq(sum * (1 - decay))
    }
    values flatMap { v => emit(v) }
  }

  //Computes the average absolute deviation of a sequence of numbers
  def averageAbsoluteDeviation(values: Seq[Double]): Double = {
    val mean = values.sum / values.size
    val absoluteDeviations = values.map(v => math.abs(v - mean))
    absoluteDeviations.sum / absoluteDeviations.size
  }

  //Computes the standard deviation of a sequence of numbers
  def standardDeviation(values: Seq[Double]): Double = {
    val mean = values.sum / values.size
    val squaredDeviations = values.map(v => {
      val deviation = v - mean
      deviation * deviation
    })
    math.sqrt(squaredDeviations.sum / squaredDeviations.size)
  }

  //Computes the mean absolute error of a sequence of numbers
  def meanAbsoluteError(values: Seq[Double]): Double = {
    val size = values.size
    val sumAbsoluteErrors = values.foldLeft(0.0) { (sum, v) =>
      sum + math.abs(v)
    }
    sumAbsoluteErrors / size
  }

  //Computes the mean squared error of a sequence of numbers
  def meanSquaredError(values: Seq[Double]): Double = {
    val size = values.size
    val sumSquaredErrors = values.foldLeft(0.0) { (sum, v) =>
      sum + v * v
    }
    sumSquaredErrors / size
  }

  //Computes the root mean squared error of a sequence of numbers
  def rootMeanSquaredError(values: Seq[Double]): Double = {
    val size = values.size
    val sumSquaredErrors = values.foldLeft(0.0) { (sum, v) =>
      sum + v * v
    }
    math.sqrt(sumSquaredErrors / size)
  }

  //Computes the mean absolute percentage error of a sequence of numbers
  def meanAbsolutePercentageError(values: Seq[Double]): Double = {
    val size = values.size
    val sumAbsolutePercentageErrors = values.foldLeft(0.0) { (sum, v) =>
      sum + math.abs(v)
    }
    sumAbsolutePercentageErrors / size
  }

  //Computes the norm of a sequence of numbers
  def norm(values: Seq[Double],
               p: Double = 2.0): Double = {
    val sum = values.foldLeft(0.0) { (sum, v) =>
      sum + math.pow(math.abs(v), p)
    }
    math.pow(sum, 1.0 / p)
  }

  //Computes the cross-correlation of two sequences
  def crossCorrelation(x: Seq[Double], 
                        y: Seq[Double]): Double = {
    val n = x.length
    val xSum = x.sum
    val ySum = y.sum
    val xyProdSum = (x zip y).map { case (xVal, yVal) => xVal * yVal }.sum
    val xxProdSum = (x zip x).map { case (xVal, xVal2) => xVal * xVal2 }.sum
    val yyProdSum = (y zip y).map { case (yVal, yVal2) => yVal * yVal2 }.sum
    (n * xyProdSum - xSum * ySum) / 
      math.sqrt((n * xxProdSum - xSum * xSum) * 
        (n * yyProdSum - ySum * ySum))
    }

  //Computes the Pearson's correlation of two sequences
  def pearsonCorrelation(x: Seq[Double], 
                            y: Seq[Double]): Double = {
    val n = x.length
    val xSum = x.sum
    val ySum = y.sum
    val xyProdSum = (x zip y).map { case (xVal, yVal) => xVal * yVal }.sum
    val xxProdSum = (x zip x).map { case (xVal, xVal2) => xVal * xVal2 }.sum
    val yyProdSum = (y zip y).map { case (yVal, yVal2) => yVal * yVal2 }.sum
    (n * xyProdSum - xSum * ySum) / 
      math.sqrt((n * xxProdSum - xSum * xSum) * 
        (n * yyProdSum - ySum * ySum))
    }

  //Computes the cosine similarity of two sequences
  def cosineSimilarity(x: Seq[Double], 
                          y: Seq[Double]): Double = {
    val n = x.length
    val xSum = x.sum
    val ySum = y.sum
    val nxySum = (x zip y).map { case (xVal, yVal) => xVal * yVal }.sum
    val xxSum = (x zip x).map { case (xVal, xVal2) => xVal * xVal2 }.sum
    val yySum = (y zip y).map { case (yVal, yVal2) => yVal * yVal2 }.sum
    (nxySum / math.sqrt(xxSum * yySum))
  }

  //Computes the Jaccard similarity of two sequences
  def jaccardSimilarity(x: Seq[Double], 
                            y: Seq[Double]): Double = {
    val xyIntersection = (x zip y).map { case (xVal, yVal) => 
      math.min(xVal, yVal)
    }.sum
    val xxUnion = (x zip x).map { case (xVal, xVal2) => 
      math.max(xVal, xVal2)
    }.sum
    val yyUnion = (y zip y).map { case (yVal, yVal2) => 
      math.max(yVal, yVal2)
    }.sum
    xyIntersection / (xxUnion + yyUnion - xyIntersection)
  }

  //Computes the Spearman correlation of two sequences
  def spearmanCorrelation(x: Seq[Double], 
                            y: Seq[Double]): Double = {
    val n = x.length
    val xs = x.sorted
    val ys = y.sorted

    def rank[T](x: Seq[T], xs: Seq[T]): Seq[Double] = {
      x.map { v =>
        (xs.zipWithIndex.collect {
          case (v2, i) if v == v2 => i
        } :+ (xs.size - 1)).min.toDouble
      }
    }

    val xranks = rank(x, xs)
    val yranks = rank(y, ys)

    val xyProdSum = (xranks zip yranks).map { case (xVal, yVal) => xVal * yVal }.sum
    val xxProdSum = (xranks zip xranks).map { case (xVal, xVal2) => xVal * xVal2 }.sum
    val yyProdSum = (yranks zip yranks).map { case (yVal, yVal2) => yVal * yVal2 }.sum
    (n * xyProdSum - xranks.sum * yranks.sum) / 
      math.sqrt((n * xxProdSum - xranks.sum * xranks.sum) * 
        (n * yyProdSum - yranks.sum * yranks.sum))
  }

  //Computes the Kendall correlation of two sequences
  def kendallCorrelation(x: Seq[Double], 
                            y: Seq[Double]): Double = {
    val n = x.length
    val xs = x.sorted
    val ys = y.sorted

    def rank[T](x: Seq[T], xs: Seq[T]): Seq[Double] = {
      x.map { v =>
        (xs.zipWithIndex.collect {
          case (v2, i) if v == v2 => i
        } :+ (xs.size - 1)).min.toDouble
      }
    }

    val xranks = rank(x, xs)
    val yranks = rank(y, ys)

    val xyProdSum = (xranks zip yranks).map { case (xVal, yVal) => xVal * yVal }.sum
    val xxProdSum = (xranks zip xranks).map { case (xVal, xVal2) => xVal * xVal2 }.sum
    val yyProdSum = (yranks zip yranks).map { case (yVal, yVal2) => yVal * yVal2 }.sum
    (n * xyProdSum - xranks.sum * yranks.sum) / 
      math.sqrt((n * xxProdSum - xranks.sum * xranks.sum) * 
        (n * yyProdSum - yranks.sum * yranks.sum))
  }

  //Computes the modified z-score of a sequence of numbers
  def modifiedZScore(values: Seq[Double]): Double = {
    val mean = values.sum / values.size
    val absoluteDeviations = values.map(v => math.abs(v - mean))
    val medianDeviation = absoluteDeviations.sum / absoluteDeviations.size
    (values.map(_ - mean) :/ medianDeviation).head
  }

  //Computes the median absolute deviation (MAD) of a sequence of numbers
  def medianAbsoluteDeviation(values: Seq[Double]): Double = {
    val median = values.sum / values.size
    val absoluteDeviations = values.map(v => math.abs(v - median))
    absoluteDeviations.sum / absoluteDeviations.size
  }

  //Computes the median of a sequence of numbers
  def median(values: Seq[Double]): Double = {
    val (lower, upper) = values.sortWith(_ < _).splitAt(values.size / 2)
    if (values.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }

  //Computes the mode of a sequence of numbers
  def mode(values: Seq[Double]): Double = {
    val frequencies = values.groupBy(x => x).mapValues(_.length)
    values.maxBy(x => frequencies(x))
  }

  //Computes the range of a sequence of numbers
  def range(values: Seq[Double]): Double = {
    values.max - values.min
  }
}