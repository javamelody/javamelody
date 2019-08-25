<%@ taglib uri="javamelody-prometheus" prefix="prometheus" %>
<%@ page contentType="text/plain; version=0.0.4;charset=UTF-8" %>
<%@ page trimDirectiveWhitespaces="true" %>
<prometheus:mbean jmxValue="java.lang:type=OperatingSystem.ProcessCpuTime" metricName="process_cpu_nanos_total" metricType="counter" />
<prometheus:custom metricName="memory_used_bytes" metricType="gauge">
	<%= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory() %>
</prometheus:custom>
<prometheus:request requestId="http9b051a0212745888c373b46ee5f27d9d6d905d45" />
<prometheus:standard includeLastValue="false"/>