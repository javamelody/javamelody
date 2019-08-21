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

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.BodyTagSupport;

public class PrometheusCustomTag extends BodyTagSupport {
	private static final long serialVersionUID = 1L;

	public enum MetricType {
		counter, gauge, histogram, summary
	}

	private String metricName;
	private MetricType metricType;
	private String metricHelp;

	public String getMetricName() {
		return metricName;
	}

	public void setMetricName(String metricName) {
		this.metricName = metricName;
	}

	public MetricType getMetricType() {
		return metricType;
	}

	public void setMetricType(MetricType metricType) {
		this.metricType = metricType;
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
		try {
			assert metricName != null;
			assert metricType != null;
			if (metricHelp != null) {
				println("# HELP ", metricName, " ", metricHelp);
			}
			println("# TYPE ", metricName, " ", metricType.name());
			println(metricName, " ", getBodyContent().getString().trim());
		} catch (final IOException e) {
			throw new JspException(e);
		}
		return super.doEndTag();
	}

	private void println(String... strings) throws IOException {
		final JspWriter out = pageContext.getOut();
		for (final String s : strings) {
			out.print(s);
		}
		out.print('\n');
	}
}
