package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.MemberApi;
import com.uket.domain.auth.admin.dto.SearchAdminDto;
import com.uket.app.admin.api.dto.response.SearchAdminsResponse;
import com.uket.domain.auth.admin.service.AdminService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class MemberController implements MemberApi {

    private final AdminService adminService;

    @Override
    public ResponseEntity<SearchAdminsResponse> searchAllAdmins(int page, int size) {
        PageRequest pageRequest = PageRequest.of(page - 1, size);
        Page<SearchAdminDto> dtoPage = adminService.searchAllAdmins(pageRequest);
        List<SearchAdminDto> dtoList = dtoPage.getContent();
        return ResponseEntity.ok(SearchAdminsResponse.from(dtoList));
    }

}
