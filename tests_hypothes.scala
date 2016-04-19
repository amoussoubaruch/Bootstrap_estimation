/**
   * Tests d'hypotheses & boostrap
   * Scala 
   */
 
import scala.util.Sorting.quickSort

val dataWithHeader = sc.textFile("zipped/skewdata.csv")
val header = dataWithHeader.first
val data = dataWithHeader.filter( _ != header ).map( _.toDouble )

def getConfInterval(input: org.apache.spark.rdd.RDD[Double], N: Int, left: Double, right:Double)
			: (Double, Double) = {
	// Simulate by sampling and calculating averages for each of subsamples
	val hist = Array.fill(N){0.0}
	for (i <- 0 to N-1) {
		hist(i) = input.sample(withReplacement = true, fraction = 1.0).mean
	}

	// Sort the averages and calculate quantiles
	quickSort(hist)

	val left_quantile  = hist((N*left).toInt)
	val right_quantile = hist((N*right).toInt)

	return (left_quantile, right_quantile)
}

val (left_qt, right_qt) = getConfInterval(data, 1000, 0.025, 0.975)

val H0_mean = 30

if (left_qt < H0_mean && H0_mean < right_qt) {
	println("We failed to reject H0. It seems like H0 is correct.")
} else {
	println("We rejected H0")
}


def getConfIntervalTwoMeans(input1: org.apache.spark.rdd.RDD[Double], 
					input2: org.apache.spark.rdd.RDD[Double], 
					N: Int, left: Double, right:Double)
            : (Double, Double) = {
    // Simulate average of differences
	val hist = Array.fill(N){0.0}
	for (i <- 0 to N-1) {
		val mean1 = input1.sample(withReplacement = true, fraction = 1.0).mean
		val mean2 = input2.sample(withReplacement = true, fraction = 1.0).mean
		hist(i) = mean2 - mean1
	}

	// Sort the averages and calculate quantiles
	quickSort(hist)

	val left_quantile  = hist((N*left).toInt)
	val right_quantile = hist((N*right).toInt)

	return (left_quantile, right_quantile)
}

// Let's try to check the same dataset with itself. Ha-ha.
val (left_qt, right_qt) = getConfIntervalTwoMeans(data, data, 1000, 0.025, 0.975)

// A condition was changed because of one-tailed test.
if (left_qt > 0) {
	println("We failed to reject H0. It seems like H0 is correct.")
} else {
	println("We rejected H0")
}


