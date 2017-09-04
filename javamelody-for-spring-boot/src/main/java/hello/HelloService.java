package hello;

import org.springframework.stereotype.Service;

@SuppressWarnings("javadoc")
@Service
public class HelloService {
	public String hello() {
		return "Greetings from Spring Boot!";
	}
}
