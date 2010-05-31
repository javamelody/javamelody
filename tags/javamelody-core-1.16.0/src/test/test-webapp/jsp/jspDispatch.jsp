<%@ page session="false" %>
<jsp:include page="fast.jsp"></jsp:include>

<% request.getRequestDispatcher("fast.jsp").include(request, response); %>
