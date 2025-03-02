package com.uket.app.ticket.api;

import com.uket.modules.redis.config.RedisConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;

@SpringBootTest
@Import({RedisConfig.class})
class TicketAppApiApplicationTests {

    @Test
    void contextLoads() {
    }

}
