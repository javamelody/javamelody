<%@ page session="false" %>
<%@page import="java.util.Random"%>
<%@page import="java.util.Date"%>
<%@page import="org.quartz.*"%>
<%@page import="org.quartz.impl.StdSchedulerFactory"%>
<%@page import="net.bull.javamelody.JobTestImpl"%>
<%
//Grab the Scheduler instance from the Factory 
Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

// and start it off
scheduler.start();

//Define job instance
Random random = new Random();

// Trigger the job to run now, and then repeat every 40 seconds

JobDetail job = JobBuilder.newJob(JobTestImpl.class).withIdentity("job" + random.nextInt()).withDescription("coucou\ncoucou").build();
/* job.setDescription("coucou\ncoucou");
 */
//Define a Trigger that will fire "now"
Trigger trigger = TriggerBuilder.newTrigger().withIdentity("trigger" + random.nextInt()).startNow().build();
//Schedule the job with the trigger
scheduler.scheduleJob(job, trigger);

//Define a Trigger that will fire "later"
JobDetail job2 = JobBuilder.newJob(JobTestImpl.class).withIdentity("job" + random.nextInt()).build();
Trigger trigger3 = TriggerBuilder.newTrigger().withIdentity("trigger" + random.nextInt())
	.startAt(new Date(System.currentTimeMillis() + 10000)).build();
scheduler.scheduleJob(job2, trigger3);
%>

Jobs initialized

<br/>
<a href="../index.jsp">back</a>
