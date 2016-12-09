package net.bull.javamelody;

import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Used to expose CounterRequestAggregationData via JMX
 *
 * @see CounterRequestAggregationData
 *
 * @author Alexey Pushkin
 */
public class CounterRequestAggregationData {
    private CounterRequestData globalRequest;
    private CounterRequestData warningRequest;
    private CounterRequestData severeRequest;
    private int warningThreshold;
    private int severeThreshold;
    private SortedMap<String, CounterRequestData> requests;

    public CounterRequestAggregationData (CounterRequestAggregation aggregation) {
        this.globalRequest = new CounterRequestData(aggregation.getGlobalRequest());
        this.warningRequest = new CounterRequestData(aggregation.getWarningRequest());
        this.severeRequest = new CounterRequestData(aggregation.getSevereRequest());
        this.warningThreshold = aggregation.getWarningThreshold();
        this.severeThreshold = aggregation.getSevereThreshold();

        this.requests = new TreeMap<String, CounterRequestData>();
        List<CounterRequest> requestList = aggregation.getRequests();
        for (CounterRequest request: requestList) {
            requests.put(request.getName(), new CounterRequestData(request));
        }
    }

    public CounterRequestData getGlobalRequest() {
        return globalRequest;
    }

    public CounterRequestData getWarningRequest() {
        return warningRequest;
    }

    public CounterRequestData getSevereRequest() {
        return severeRequest;
    }

    public int getWarningThreshold() {
        return warningThreshold;
    }

    public int getSevereThreshold() {
        return severeThreshold;
    }

    public SortedMap<String, CounterRequestData> getRequests() {
        return requests;
    }
}
