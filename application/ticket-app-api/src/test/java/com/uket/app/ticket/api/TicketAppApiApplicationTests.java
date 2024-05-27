package com.uket.app.ticket.api;

import com.uket.modules.redis.lock.config.RedissonConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;

@SpringBootTest
@Import({RedissonConfig.class})
class TicketAppApiApplicationTests {

	@Test
	void contextLoads() {
	}

}
