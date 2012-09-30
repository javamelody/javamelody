import javax.servlet.*;
import javax.servlet.http.*;
import java.io.IOException;

public class ForbiddenActionsFilter implements Filter {
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		if ("kill_thread".equalsIgnoreCase(request.getParameter("action"))) {
			((HttpServletResponse) response).sendError(HttpServletResponse.SC_FORBIDDEN, "Kill thread forbidden in the demo");
			return;
		} else if ("heap_dump".equalsIgnoreCase(request.getParameter("action"))) {
			((HttpServletResponse) response).sendError(HttpServletResponse.SC_FORBIDDEN, "Heap dump forbidden in the demo");
			return;
		} else if ("stop".equalsIgnoreCase(request.getParameter("collector"))) {
			((HttpServletResponse) response).sendError(HttpServletResponse.SC_FORBIDDEN, "Collector forbidden in the demo");
			return;
		}
		chain.doFilter(request, response);
	}
	
	public void init(FilterConfig config) {
	}
	
	public void destroy() {
	}
}
