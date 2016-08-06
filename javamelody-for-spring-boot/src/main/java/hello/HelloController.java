package hello;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
// @MonitoredWithSpring (not necessary since controllers are monitored by default in JavaMelodyConfiguration)
@SuppressWarnings("javadoc")
public class HelloController {

	@RequestMapping("/")
	public String index() {
		return "Greetings from Spring Boot!";
	}

}
