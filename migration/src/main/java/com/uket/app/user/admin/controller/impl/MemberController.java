package com.uket.app.user.admin.controller.impl;

import com.uket.app.user.admin.controller.MemberApi;
import com.uket.app.user.admin.dto.SearchAdminDto;
import com.uket.app.user.admin.dto.response.SearchAdminsResponse;
import com.uket.app.user.admin.service.AdminService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class MemberController implements MemberApi {

    private AdminService adminService;

    @Override
    public ResponseEntity<SearchAdminsResponse> searchAllAdmins(int page, int size) {
        PageRequest pageRequest = PageRequest.of(page - 1, size);
        Page<SearchAdminDto> dtoPage = adminService.searchAllAdmins(pageRequest);
        List<SearchAdminDto> dtoList = dtoPage.getContent();
        return ResponseEntity.ok(SearchAdminsResponse.from(dtoList));
    }

}
