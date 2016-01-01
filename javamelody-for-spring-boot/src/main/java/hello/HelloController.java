package hello;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import net.bull.javamelody.MonitoredWithSpring;

@RestController
@MonitoredWithSpring
@SuppressWarnings("javadoc")
public class HelloController {

	@RequestMapping("/")
	public String index() {
		return "Greetings from Spring Boot!";
	}

}
