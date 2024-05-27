package com.uket.app.ticket.api;

import com.uket.modules.redis.lock.config.RedissonConfig;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@Import({RedissonConfig.class})
class TicketAppApiApplicationTests {

	@Test
	void contextLoads() {
	}

}
