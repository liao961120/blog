library(htmltools)

######## Button Created by <a> #############
button_a <- function(text, class="primary", role="button") {
    class <- paste("btn-", class, sep="")
    a(role=role, class=paste("btn", class, sep = " "),
           text)
}

####### Button by <button> #########
## positioning: class="pull-right"

button_bs <- function(text, class="primary", data_dismiss=NULL,
data_toggle=NULL, data_target=NULL, data_parent=NULL,
aria_expanded=NULL, aria_controls=NULL) {
    class <- paste("btn", paste("btn-", class, sep=""))
    tags$button(type="button", class=class,
                # Modal/collapse Control
                `data-parent`=data_parent,
                `data-toggle`=data_toggle, #open modal
                `data-target`=data_target, #modal ID
                # collapse
                `aria-expanded`=aria_expanded,
                `aria-controls`=aria_controls,
                `data-dismiss`=data_dismiss,
                text)
}

################# Panel #####################
panel <- function(content, title, color = "default") {
    color <- paste("panel", paste("panel-", color, sep = ""))
    div(class = color,
        div(class = "panel-heading",
            h3(class = "panel-title", title)),
        div(class = "panel-body", content))
}

############### Panel-group ###############
panel_group <- function(id, p1, p2=NULL, p3=NULL, 
                        p4=NULL, p5=NULL, p6=NULL, 
                        p7=NULL, p8=NULL, p9=NULL,
                        p10=NULL) {
    div(class="panel-group", id=id,
        role="tablist", `aria-multiselectable`="true",p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
}

panel_sub <- function(col_tar, id_head="head", aria_label, data_parent, title, content, 
                      color = "default",
                      panel_coll = "panel-collapse collapse") {
    color <- paste("panel", paste("panel-", color, sep = ""))
    div(class = color,
        div(class = "panel-heading", role="tab", id=id_head,
            h4(class = "panel-title", 
               a(role="button", `data-toggle`="collapse", `aria-expanded`="true",
                 `data-parent`=data_parent, href=paste("#",col_tar,sep=""),
                 `aria-controls`=col_tar,
                 title)
               )
            ),
        div(id = col_tar,
            class = panel_coll,
            role = "tabpanel" ,
            `aria-labelledby` = aria_label,
            div(class = "panel-body", content)
        )
    )
}

################# Collapse ####################
### color: alert alert-success|nfo|warning|danger
#
####### Collapse Button ##################
### button_bs("Button_text", class="info", data_toggle="collapse", aria_expanded="false", aria_controls="id", data_target = "#id")
##########################################

collapse <- function(content, id, color="well", class="collapse") {
    div(class=class, id=id,
        div(class=color, content)
    )
}

########### Modal ############
## size: modal-lg | modal-sm
modal <-
    function(modal_header, id, modal_body, foot=T, head=T,
             modal_body2 = NULL, modal_body3= NULL,
             modal_body4= NULL, modal_body5= NULL,
             modal_body6= NULL, modal_footer =NULL, 
             size = NULL) {
        modal_size <- paste("modal-dialog", size, sep = " ")
        
        # modal
        div(id = id,
            class = "modal fade",
            role = "dialog",
            tabindex = "-1",
            div(class = modal_size,
                role = "document",
                # modal content
                div(class = "modal-content",
                        div(class = "modal-header",
                            tags$button(type = "button",
                                        class = "close",
                                        `data-dismiss` = "modal",
                                        HTML("&times;")
                            ),
                            h4(class = "modal-title", modal_header)
                        ),
                    div(class = "modal-body",
                        modal_body, modal_body2,
                        modal_body3, modal_body4,
                        modal_body5, modal_body6),
                    if (foot==T){
                        div(class = "modal-footer",
                            button_bs("Close", class = "default",
                                      data_dismiss = "modal"),
                            modal_footer
                        )
                    }
                )
            )
        )
    }


