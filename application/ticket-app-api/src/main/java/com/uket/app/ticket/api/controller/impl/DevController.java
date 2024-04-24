package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.DevApi;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
public class DevController implements DevApi {

    @Override
    public ResponseEntity<String> test(Long userId) {
        return ResponseEntity.ok("userId: " + userId);
    }
}
