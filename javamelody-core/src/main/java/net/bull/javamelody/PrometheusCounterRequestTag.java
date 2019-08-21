/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.TagSupport;

import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;

public class PrometheusCounterRequestTag extends TagSupport {
	private static final long serialVersionUID = 1L;

	private String requestId;
	private String metricHelp;

	public String getRequestId() {
		return requestId;
	}

	public void setRequestId(String requestId) {
		this.requestId = requestId;
	}

	public String getMetricHelp() {
		return metricHelp;
	}

	public void setMetricHelp(String metricHelp) {
		this.metricHelp = metricHelp;
		if (this.metricHelp != null) {
			this.metricHelp = this.metricHelp.trim().replace('\n', ' ').replace('\r', ' ');
		}
	}

	@Override
	public int doEndTag() throws JspException {
		final ServletContext servletContext = pageContext.getServletContext();
		final FilterContext filterContext = (FilterContext) servletContext
				.getAttribute(ReportServlet.FILTER_CONTEXT_KEY);
		final Collector collector = filterContext.getCollector();
		for (final Counter counter : collector.getCounters()) {
			if (counter.isDisplayed() && counter.isRequestIdFromThisCounter(requestId)) {
				final CounterRequest counterRequest = counter.getCounterRequestById(requestId);
				try {
					printRequest(counter, counterRequest);
				} catch (final IOException e) {
					throw new JspException(e);
				}
				return super.doStartTag();
			}
		}

		throw new IllegalArgumentException("Request not found : " + requestId);
	}

	private void printRequest(Counter counter, CounterRequest counterRequest) throws IOException {
		final long hits = counterRequest.getHits();
		final long errors = counterRequest.getSystemErrors();
		final long duration = counterRequest.getDurationsSum();
		if (metricHelp != null) {
			println("# HELP ", requestId, "_hits_count ", metricHelp);
		}
		println("# TYPE ", requestId, "_hits_count counter");
		println(requestId, "_hits_count ", String.valueOf(hits));
		if (!counter.isErrorCounter() || counter.isJobCounter()) {
			// errors has no sense for the error and log counters
			if (metricHelp != null) {
				println("# HELP ", requestId, "_errors_count ", metricHelp);
			}
			println("# TYPE ", requestId, "_errors_count counter");
			println(requestId, "_errors_count ", String.valueOf(errors));
		}
		if (duration >= 0) {
			// duration is negative and has no sense for the log counter
			if (metricHelp != null) {
				println("# HELP ", requestId, "_duration_millis ", metricHelp);
			}
			println("# TYPE ", requestId, "_duration_millis counter");
			println(requestId, "_duration_millis ", String.valueOf(duration));
		}
	}

	private void println(String... strings) throws IOException {
		final JspWriter out = pageContext.getOut();
		for (final String s : strings) {
			out.print(s);
		}
		out.print('\n');
	}
}
