package net.bull.javamelody;

/**
 * MXBean which exposes CounterRequestAggregation data via JMX
 * @see javax.management.MXBean
 * @see CounterRequestAggregation
 *
 *
 * @author Alexey Pushkin
 */
public interface CounterRequestMXBean {
    CounterRequestAggregationData getCounterRequestAggregation();
}
