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
import java.util.Collections;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.TagSupport;

import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.web.MonitoringController;

public class PrometheusStandardTag extends TagSupport {
	private static final long serialVersionUID = 1L;

	private boolean includeLastValue;

	public boolean isIncludedLastValue() {
		return includeLastValue;
	}

	public void setIncludeLastValue(boolean includeLastValue) {
		this.includeLastValue = includeLastValue;
	}

	@Override
	public int doEndTag() throws JspException {
		final ServletContext servletContext = pageContext.getServletContext();
		final JavaInformations javaInformations = new JavaInformations(servletContext, true);
		final FilterContext filterContext = (FilterContext) servletContext
				.getAttribute(ReportServlet.FILTER_CONTEXT_KEY);
		final Collector collector = filterContext.getCollector();
		final HttpServletResponse httpResponse = (HttpServletResponse) pageContext.getResponse();
		final MonitoringController monitoringController = new MonitoringController(collector, null);
		try {
			monitoringController.doPrometheus(httpResponse,
					Collections.singletonList(javaInformations), includeLastValue);
		} catch (final IOException e) {
			throw new JspException(e);
		}

		return super.doEndTag();
	}
}
