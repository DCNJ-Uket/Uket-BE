package com.uket.app.ticket.api.controller;

import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "티켓 API", description = "티켓 관련 API")
@RestController
@RequestMapping("/api/v1/tickets")
@ApiResponse(responseCode = "200", description = "OK")
public interface TicketApi {


}
