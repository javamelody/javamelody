package hello;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SuppressWarnings("javadoc")
@RestController
public class HelloController {

	@Autowired
	private HelloService service;

	@RequestMapping("/")
	public String index() {
		return service.hello();
	}
}
