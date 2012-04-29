import javax.servlet.*;
import javax.servlet.http.*;
import java.io.IOException;

public class KillThreadForbiddenFilter implements Filter {
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		if ("kill_thread".equals(request.getParameter("action"))) {
			((HttpServletResponse) response).sendError(HttpServletResponse.SC_FORBIDDEN, "Kill thread forbidden in the demo");
			return;
		}
		chain.doFilter(request, response);
	}
	
	public void init(FilterConfig config) {
	}
	
	public void destroy() {
	}
}
