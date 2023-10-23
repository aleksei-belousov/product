CLASS lhc_Product DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Product RESULT result.

    METHODS activate FOR MODIFY
      IMPORTING keys FOR ACTION product~activate.

    METHODS edit FOR MODIFY
      IMPORTING keys FOR ACTION product~edit.

    METHODS resume FOR MODIFY
      IMPORTING keys FOR ACTION product~resume.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR product RESULT result.

    METHODS create_products FOR MODIFY
      IMPORTING keys FOR ACTION product~create_products.

    METHODS check_products FOR MODIFY
      IMPORTING keys FOR ACTION product~check_products.

    METHODS on_create FOR DETERMINE ON MODIFY
      IMPORTING keys FOR product~on_create.

    METHODS on_model_modify FOR DETERMINE ON MODIFY " on modify model
      IMPORTING keys FOR product~on_model_modify.

    METHODS on_scheme_save FOR DETERMINE ON SAVE "on save model, color, matrix type, country
      IMPORTING keys FOR product~on_scheme_save.

*   Internal methods:

*   Convert Sizes to Items
    METHODS sizes_to_items
      IMPORTING
        VALUE(is_draft)      TYPE abp_behv_flag
        VALUE(i_productuuid) TYPE zi_product_001-ProductUUID
        VALUE(i_model)       TYPE zi_product_001-Model
        VALUE(i_color)       TYPE zi_product_001-Color.

*   Check the Material (exists or does not) in Items
    METHODS check_product_internal
      IMPORTING
        VALUE(i_product)     TYPE string
      RETURNING
        VALUE(exists)        TYPE abap_boolean.

*   Check Materials (exists or doesn't) and Update Status on Items
    METHODS check_items_internal
      IMPORTING
        VALUE(i_productuuid) TYPE zi_product_001-ProductUUID.

*   Check Materials (exists or doesn't) and Update Status on Sizes
    METHODS check_sizes_internal
      IMPORTING
        VALUE(i_productuuid) TYPE zi_product_001-ProductUUID.


ENDCLASS. " lhc_Product DEFINITION

CLASS lhc_Product IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD Activate. " on pressing Save

    DATA it_item_create TYPE TABLE FOR CREATE zi_product_001\_Item. " Item
    DATA wa_item_create LIKE LINE OF it_item_create.
    DATA it_item_update TYPE TABLE FOR UPDATE zi_product_001\\Item. " Item
    DATA wa_item_update LIKE LINE OF it_item_update.

    DATA cid TYPE string.

    DATA plant              TYPE string.
    DATA model              TYPE string.
    DATA color              TYPE string.
    DATA cupsize            TYPE string.
    DATA backsize           TYPE string.
    DATA product            TYPE string.
    DATA quantity           TYPE string.
    DATA criticality        TYPE string.
    DATA productURL         TYPE string.

    DATA is_draft           TYPE abp_behv_flag VALUE '00'.

   "read transfered instances
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
      ENTITY Product
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Activate.' ) ) TO reported-product.

        IF ( <entity>-%is_draft = '00' ). " Saved

*           Disable Copy Color Mode and Set Link to Source Product
            DATA(copying)           = abap_false.
            DATA(sourceProductURL)  = '/ui#Material-manage&/C_Product(Product=''' && <entity>-SourceProduct && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product
                UPDATE FIELDS ( Copying SourceProductURL )
                WITH VALUE #( (
*                   %is_draft           = <entity>-%is_draft
*                    %key                = <entity>-%key
                    %tky                = <entity>-%tky
                    Copying             = copying
                    SourceProductURL    = sourceProductURL
                ) )
                FAILED DATA(ls_failed)
                MAPPED DATA(ls_mapped)
                REPORTED DATA(ls_reported).

*           Read Actual Matrix
            SELECT SINGLE * FROM zproduct001  WHERE ( productuuid = @<entity>-ProductUUID ) INTO @DATA(wa_product).

*           Read Matrix Draft
            SELECT SINGLE * FROM zproduct001d WHERE ( productuuid = @<entity>-ProductUUID ) INTO @DATA(wa_product_draft).

*           For fixing old product
            DATA(hidden22) = abap_true.
            IF ( ( <entity>-Model IS INITIAL ) OR ( <entity>-Color IS INITIAL ) ).
                hidden22 = abap_false.
            ENDIF.

            MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product
                UPDATE FIELDS ( Hidden22 )
                WITH VALUE #( (
                    %tky                = <entity>-%tky
                    Hidden22            = hidden22 " for fixing old matrix
                ) )
                FAILED DATA(ls_failed1)
                MAPPED DATA(ls_mapped1)
                REPORTED DATA(ls_reported1).

            IF ( <entity>-Copying = abap_true ). " Copy Color
*               If model changed - do not generate items (change scheme instead)
                IF ( wa_product-model <> wa_product_draft-model ).
                    RETURN.
                ENDIF.
            ELSE. " Default Behavior
*               If model/color changed - do not generate items (change scheme instead)
                IF ( ( wa_product-model <> wa_product_draft-model ) OR ( wa_product-color <> wa_product_draft-color ) ).
                    RETURN.
                ENDIF.
            ENDIF.

*           If model/color invalid - do not generate any items
*            IF ( ( <entity>-Model IS INITIAL ) OR ( <entity>-Color IS INITIAL ) ).
*                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Model/Color invalid - Fix Data.' ) ) TO reported-matrix.
*                RETURN.
*            ENDIF.

*           Generate Items - Convert Sizes To Items
            sizes_to_items(
                is_draft        = <entity>-%is_draft
                i_productuuid   = <entity>-ProductUUID
                i_model         = <entity>-Model
                i_color         = <entity>-Color
            ).

*           Check and Set Product Status on Items
            check_items_internal( <entity>-ProductUUID ).

*           Check and Set Product Status on Sizes
            check_sizes_internal( <entity>-ProductUUID ).

        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

        " After exit of the Method - Refresh Items and Sizes Tables on UI (Side Effect on _Item and _Size nodes)

    ENDLOOP.

  ENDMETHOD. " Activate

  METHOD Edit.
  ENDMETHOD.

  METHOD Resume.
  ENDMETHOD.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD create_products. " Create Products

*   Product Plant
    DATA it_mrp                         TYPE TABLE FOR READ RESULT I_ProductTP_2\\ProductPlant\_ProductPlantMRP.
    DATA it_supplyplanning              TYPE TABLE FOR READ RESULT I_ProductTP_2\\ProductPlant\_ProductPlantSupplyPlanning.
    DATA it_purchasetax                 TYPE TABLE FOR READ RESULT I_ProductTP_2\\ProductPlant\_ProductPlantPurchaseTax.

*   ProductSalesDelivery
    DATA it_salestax                    TYPE TABLE FOR READ RESULT I_ProductTP_2\\ProductSalesDelivery\_ProdSalesDeliverySalesTax.

*   Product
    DATA it_product_create              TYPE TABLE FOR CREATE I_ProductTP_2\\Product.
    DATA it_productdescription_create   TYPE TABLE FOR CREATE I_ProductTP_2\_ProductDescription.
    DATA it_productewmwarehouse_create  TYPE TABLE FOR CREATE I_ProductTP_2\_ProductEWMWarehouse.
    DATA it_productplant_create         TYPE TABLE FOR CREATE I_ProductTP_2\_ProductPlant.
    DATA it_productprocurement_create   TYPE TABLE FOR CREATE I_ProductTP_2\_ProductProcurement.
    DATA it_productqualitymanage_create TYPE TABLE FOR CREATE I_ProductTP_2\_ProductQualityManagement.
    DATA it_productsales_create         TYPE TABLE FOR CREATE I_ProductTP_2\_ProductSales.
    DATA it_productsalesdelivery_create TYPE TABLE FOR CREATE I_ProductTP_2\_ProductSalesDelivery.
    DATA it_productstorage_create       TYPE TABLE FOR CREATE I_ProductTP_2\_ProductStorage.
    DATA it_productunitofmeasure_create TYPE TABLE FOR CREATE I_ProductTP_2\_ProductUnitOfMeasure.
    DATA it_productvaluation_create     TYPE TABLE FOR CREATE I_ProductTP_2\_ProductValuation.

*   Product Plant
    DATA it_mrp_create                  TYPE TABLE FOR CREATE I_ProductTP_2\\ProductPlant\_ProductPlantMRP.
    DATA it_supplyplanning_create       TYPE TABLE FOR CREATE I_ProductTP_2\\ProductPlant\_ProductPlantSupplyPlanning.
    DATA it_purchasetax_create          TYPE TABLE FOR CREATE I_ProductTP_2\\ProductPlant\_ProductPlantPurchaseTax.

*   ProductSalesDelivery
    DATA it_salestax_create             TYPE TABLE FOR CREATE I_ProductTP_2\\ProductSalesDelivery\_ProdSalesDeliverySalesTax.

    " Read transfered instances
    READ ENTITIES OF zi_product_001  IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved

            DATA(exists) = check_product_internal( CONV string( <entity>-SourceProduct ) ).
            IF ( exists = abap_false ).
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'The Source Product not valid.' ) ) TO reported-product.
                RETURN.
            ENDIF.

*           Read Source Product
            READ ENTITIES OF I_ProductTP_2
*               Product
                ENTITY Product
                ALL FIELDS WITH VALUE #( (
                    %key-Product = <entity>-SourceProduct " '000231-048-B-035'
                ) )
                RESULT DATA(lt_sourceproduct)
                FAILED DATA(ls_failed0)
                REPORTED DATA(ls_reported0).

*           Read Source Product Nodes
            LOOP AT lt_sourceproduct INTO DATA(ls_sourceproduct).

                READ ENTITIES OF I_ProductTP_2
*                   Description
                    ENTITY Product BY \_ProductDescription
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductdescription)
*                   Product EWM Warehouse
                    ENTITY Product BY \_ProductEWMWarehouse
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductewmwarehouse)
*                   Plant
                    ENTITY Product BY \_ProductPlant
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductplant)
*                   Procurement
                    ENTITY Product BY \_ProductProcurement
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductprocurement)
*                   Quality Management
                    ENTITY Product BY \_ProductQualityManagement
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductqualitymanage)
*                   Sales
                    ENTITY Product BY \_ProductSales
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductsales)
*                   Sales Delivery
                    ENTITY Product BY \_ProductSalesDelivery
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductsalesdelivery)
*                   Storage
                    ENTITY Product BY \_ProductStorage
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductstorage)
*                   Unit Of Measure
                    ENTITY Product BY \_ProductUnitOfMeasure
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductunitofmeasure)
*                   Valuation
                    ENTITY Product BY \_ProductValuation
                    ALL FIELDS WITH VALUE #( (
                        %key-Product = ls_sourceproduct-Product
                    ) )
                    RESULT DATA(lt_sourceproductvaluation)
                    FAILED DATA(ls_failed1)
                    REPORTED DATA(ls_reported1).

*               Read Source Product Plant nodes:
                LOOP AT lt_sourceproductplant INTO DATA(ls_sourceproductplant).

                    READ ENTITIES OF I_ProductTP_2
*                       Product Plant - MRP
                        ENTITY ProductPlant BY \_ProductPlantMRP
                        ALL FIELDS WITH VALUE #( (
                            %key-Product = ls_sourceproductplant-Product " '000231-048-B-035'
                            %key-Plant   = ls_sourceproductplant-Plant   " '1010'
                        ) )
                        RESULT DATA(lt_sourceproductmrp)
*                       Product Plant - Suppling Planning
                        ENTITY ProductPlant BY \_ProductPlantSupplyPlanning
                        ALL FIELDS WITH VALUE #( (
                            %key-Product = ls_sourceproductplant-Product
                            %key-Plant   = ls_sourceproductplant-Plant
                        ) )
                        RESULT DATA(lt_sourceproductsupplyplanning)
*                       Product Plant - Purchase Tax
                        ENTITY ProductPlant BY \_ProductPlantPurchaseTax
                        ALL FIELDS WITH VALUE #( (
                            %key-Product = ls_sourceproductplant-Product
                            %key-Plant   = ls_sourceproductplant-Plant
                        ) )
                        RESULT DATA(lt_sourceproductpurchasetax)
                        FAILED DATA(ls_failed2)
                        REPORTED DATA(ls_reported2).

                    APPEND LINES OF lt_sourceproductmrp             TO it_mrp.
                    APPEND LINES OF lt_sourceproductsupplyplanning  TO it_supplyplanning.
                    APPEND LINES OF lt_sourceproductpurchasetax     TO it_purchasetax.

                ENDLOOP. " lt_sourceproductplant

*               Read Source Product sub-nodes (of Product Sales Delivery):
                LOOP AT lt_sourceproductsalesdelivery INTO DATA(ls_sourceproductsalesdelivery).
                    READ ENTITIES OF I_ProductTP_2
*                       Product Sales Delivery - Sales Tax
                        ENTITY ProductSalesDelivery BY \_ProdSalesDeliverySalesTax
                        ALL FIELDS WITH VALUE #( (
                            %key-Product                 = ls_sourceproductsalesdelivery-Product                 " '000231-048-B-035'
                            %key-ProductSalesOrg         = ls_sourceproductsalesdelivery-ProductSalesOrg         " '1010'
                            %key-ProductDistributionChnl = ls_sourceproductsalesdelivery-ProductDistributionChnl " '10'
                        ) )
                        RESULT DATA(lt_sourceproductsalestax)
                        FAILED DATA(ls_failed3)
                        REPORTED DATA(ls_reported3).

                    APPEND LINES OF lt_sourceproductsalestax TO it_salestax.

                ENDLOOP. " lt_sourceproductsalesdelivery

            ENDLOOP. " lt_sourceproduct

*           Read Items
            READ ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product BY \_Item
                ALL FIELDS WITH VALUE #( (
                    ProductUUID = <entity>-ProductUUID
                ) )
                RESULT DATA(lt_item)
                FAILED DATA(ls_failed4)
                REPORTED DATA(ls_reported4).

            LOOP AT lt_item INTO DATA(ls_item) WHERE ( Status IS INITIAL ).

                CLEAR it_product_create[].
                CLEAR it_productdescription_create[].
                CLEAR it_productewmwarehouse_create[].
                CLEAR it_productplant_create[].
                CLEAR it_productprocurement_create[].
                CLEAR it_productqualitymanage_create[].
                CLEAR it_productsales_create[].
                CLEAR it_productsalesdelivery_create[].
                CLEAR it_productstorage_create[].
                CLEAR it_productunitofmeasure_create[].
                CLEAR it_productvaluation_create[].

                CLEAR it_mrp_create[].
                CLEAR it_supplyplanning_create[].
                CLEAR it_purchasetax_create[].

                CLEAR it_salestax_create[].

                DATA cid            TYPE abp_behv_cid.
                DATA cid1           TYPE abp_behv_cid.
                DATA cid2           TYPE abp_behv_cid.

*               Product
                LOOP AT lt_sourceproduct INTO ls_sourceproduct.
                    cid = 'product' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproduct-Product = ls_item-Product. " '000231-048-B-035'
                    APPEND VALUE #(
                        %cid    = cid
                        %data   = ls_sourceproduct-%data
                    )
                    TO it_product_create[].
                ENDLOOP. " lt_sourceproduct

*               Product - Description
                LOOP AT lt_sourceproductdescription INTO DATA(ls_sourceproductdescription).
                    cid = 'description' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductdescription-Product = ls_item-Product.
                    IF ( ls_sourceproductdescription-Language = 'E' ).
                        CONCATENATE 'Descr for' ls_item-Product INTO ls_sourceproductdescription-ProductDescription SEPARATED BY space. " 'Descr for 000231-048-B-035'
                    ENDIF.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductdescription-Product
                        %target = VALUE #( (
                            %cid    = cid " 'description1'
                            %data   = ls_sourceproductdescription-%data
                        ) )
                    )
                    TO it_productdescription_create[].
                ENDLOOP. " lt_sourceproductdescription

*               Product - EWM Warehouse
                LOOP AT lt_sourceproductewmwarehouse INTO DATA(ls_sourceproductewmwarehouse).
                    cid = 'ewmwarehouse' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductdescription-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductewmwarehouse-Product
                        %target = VALUE #( (
                            %cid    = cid " 'ewmwarehouse1'
                            %data   = ls_sourceproductewmwarehouse-%data
                        ) )
                    )
                    TO it_productewmwarehouse_create[].
                ENDLOOP. " lt_sourceproductewmwarehouse

*               Product - Plant
                LOOP AT lt_sourceproductplant INTO ls_sourceproductplant.
                    cid1 = 'plant' && CONV string( sy-tabix ).
                    CONDENSE cid1.
                    ls_sourceproductplant-Product = ls_item-Product. " New Product
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductplant-Product
                        %target = VALUE #( (
                            %cid    = cid1 " 'plant1'
                            %data   = ls_sourceproductplant-%data
                        ) )
                    )
                    TO it_productplant_create[].
*                   Product - Plant - MRP
                    LOOP AT it_mrp INTO DATA(wa_mrp) WHERE ( Plant = ls_sourceproductplant-Plant ).
                        cid2 = 'mrp' && CONV string( sy-tabix ).
                        CONDENSE cid2.
                        wa_mrp-Product = ls_item-Product.
                        APPEND VALUE #(
                            %cid_ref        = cid1 " 'plant1'
                            %key-Product    = wa_mrp-Product
                            %key-Plant      = wa_mrp-Plant
                            %target = VALUE #( (
                                %cid    = cid2 " 'mrp1'
                                %data   = wa_mrp-%data
                            ) )
                        )
                        TO it_mrp_create[].
                    ENDLOOP.
*                   Product Plant - Supply Planning
                    LOOP AT it_supplyplanning INTO DATA(wa_supplyplanning) WHERE ( Plant = ls_sourceproductplant-Plant ).
                        cid2 = 'supplyplanning' && CONV string( sy-tabix ).
                        CONDENSE cid2.
                        wa_supplyplanning-Product = ls_item-Product.
                        APPEND VALUE #(
                            %cid_ref        = cid1 " 'plant1'
                            %key-Product    = wa_supplyplanning-Product
                            %key-Plant      = wa_supplyplanning-Plant
                            %target = VALUE #( (
                                %cid    = cid2 " 'supplyplanning1'
                                %data   = wa_supplyplanning-%data
                            ) )
                        )
                        TO it_supplyplanning_create[].
                    ENDLOOP.
*                   Product Plant - Purchase Tax
                    LOOP AT it_purchasetax INTO DATA(wa_purchasetax) WHERE ( Plant = ls_sourceproductplant-Plant ).
                        cid2 = 'purchasetax' && CONV string( sy-tabix ).
                        CONDENSE cid2.
                        wa_purchasetax-Product = ls_item-Product.
                        APPEND VALUE #(
                            %cid_ref        = cid1 " 'plant1'
                            %key-Product    = wa_purchasetax-Product
                            %key-Plant      = wa_purchasetax-Plant
                            %target = VALUE #( (
                                %cid    = cid2 " 'purchasetax1'
                                %data   = wa_purchasetax-%data
                            ) )
                        )
                        TO it_purchasetax_create[].
                    ENDLOOP.

                ENDLOOP. " lt_sourceproductplant

*               Product - Procurement
                LOOP AT lt_sourceproductprocurement INTO DATA(ls_sourceproductprocurement).
                    cid = 'procurement' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductprocurement-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductprocurement-Product
                        %target = VALUE #( (
                            %cid    = cid " 'procurement1'
                            %data   = ls_sourceproductprocurement-%data
                        ) )
                    )
                    TO it_productprocurement_create[].
                ENDLOOP. " lt_sourceproductprocurement

*               Product - Quality Manage
                LOOP AT lt_sourceproductqualitymanage INTO DATA(ls_sourceproductqualitymanage).
                    cid = 'qualitymanage' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductqualitymanage-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductqualitymanage-Product
                        %target = VALUE #( (
                            %cid    = cid " 'qualitymanage1'
                            %data   = ls_sourceproductqualitymanage-%data
                        ) )
                    )
                    TO it_productqualitymanage_create[].
                ENDLOOP. " lt_sourceproductqualitymanage

*               Product - Sales
                LOOP AT lt_sourceproductsales INTO DATA(ls_sourceproductsales).
                    cid = 'sales' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductsales-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductsales-Product
                        %target = VALUE #( (
                            %cid    = cid " 'sales1'
                            %data   = ls_sourceproductsales-%data
                        ) )
                    )
                    TO it_productsales_create[].
                ENDLOOP. " lt_sourceproductsales

*               Product - Sales Delivery
                LOOP AT lt_sourceproductsalesdelivery INTO ls_sourceproductsalesdelivery.
                    cid1 = 'salesdelivery' && CONV string( sy-tabix ).
                    CONDENSE cid1.
                    ls_sourceproductsalesdelivery-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductsalesdelivery-Product
                        %target = VALUE #( (
                            %cid    = cid1 " 'salesdelivery1'
                            %data   = ls_sourceproductsalesdelivery-%data
                        ) )
                    )
                    TO it_productsalesdelivery_create[].
*                   Product - Sales Delivery - Sales Tax
                    LOOP AT it_salestax INTO DATA(wa_salestax) WHERE ( ProductSalesOrg          = ls_sourceproductsalesdelivery-ProductSalesOrg ) AND
                                                                     ( ProductDistributionChnl  = ls_sourceproductsalesdelivery-ProductDistributionChnl ).
                        cid2 = 'salestax' && CONV string( sy-tabix ).
                        CONDENSE cid2.
                        wa_salestax-Product = ls_item-Product.
                        APPEND VALUE #(
                            %cid_ref                        = cid1 " 'salesdelivery1'
                            %key-Product                    = wa_salestax-Product
                            %key-ProductSalesOrg            = wa_salestax-ProductSalesOrg
                            %key-ProductDistributionChnl    = wa_salestax-ProductDistributionChnl
                            %target = VALUE #( (
                                %cid    = cid2 " 'salestax1'
                                %data   = wa_salestax-%data
                            ) )
                        )
                        TO it_salestax_create[].
                    ENDLOOP.
                ENDLOOP. " lt_sourceproductsalesdelivery

*               Storage
                LOOP AT lt_sourceproductstorage INTO DATA(ls_sourceproductstorage).
                    cid = 'storage' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductstorage-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductstorage-Product
                        %target = VALUE #( (
                            %cid    = cid " 'storage1'
                            %data   = ls_sourceproductstorage-%data
                        ) )
                    )
                    TO it_productstorage_create[].
                ENDLOOP. " lt_sourceproductstorage

*               Unit Of Measure
                LOOP AT lt_sourceproductunitofmeasure INTO DATA(ls_sourceproductunitofmeasure).
                    cid = 'unitofmeasure' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductunitofmeasure-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductunitofmeasure-Product
                        %target = VALUE #( (
                            %cid    = cid " 'unitofmeasure1'
                            %data   = ls_sourceproductunitofmeasure-%data
                        ) )
                    )
                    TO it_productunitofmeasure_create[].
                ENDLOOP. " lt_sourceproductunitofmeasure

*               Valuation
                LOOP AT lt_sourceproductvaluation INTO DATA(ls_sourceproductvaluation).
                    cid = 'valuation' && CONV string( sy-tabix ).
                    CONDENSE cid.
                    ls_sourceproductvaluation-Product = ls_item-Product.
                    APPEND VALUE #(
                        %cid_ref        = 'product1'
                        %key-Product    = ls_sourceproductvaluation-Product
                        %target = VALUE #( (
                            %cid    = cid " 'valuation1'
                            %data   = ls_sourceproductvaluation-%data
                        ) )
                    )
                    TO it_productvaluation_create[].
                ENDLOOP. " lt_sourceproductvaluation

*               Create a New Product (with nodes)
                MODIFY ENTITIES OF I_ProductTP_2
*                   Product
                    ENTITY Product
                    CREATE FIELDS (
                        ANPCode
                        ArticleCategory
                        AuthorizationGroup
                        BaseUnit
                        BaseUnitSpecificProductHeight
                        BaseUnitSpecificProductLength
                        BaseUnitSpecificProductWidth
                        BasicProduct
*                        CreatedByUser
*                        CreationDate
                        CreationDateTime
*                        CreationTime
                        CrossPlantConfigurableProduct
                        CrossPlantStatus
                        CrossPlantStatusValidityDate
                        DangerousGoodsIndProfile
                        DiscountInKindEligibility
                        Division
                        DocumentIsCreatedByCAD
                        ExternalProductGroup
                        GrossWeight
                        HandlingIndicator
                        HandlingUnitType
                        HasVariableTareWeight
                        IndustrySector
                        IndustryStandardName
                        InternationalArticleNumberCat
                        IsApprovedBatchRecordReqd
                        IsBatchManagementRequired
                        IsMarkedForDeletion
                        IsPilferable
                        IsRelevantForHzdsSubstances
                        ItemCategoryGroup
                        LaboratoryOrDesignOffice
*                        LastChangeDate
*                        LastChangeDateTime
*                        LastChangeTime
*                        LastChangedByUser
                        MaximumCapacity
                        MaximumPackagingHeight
                        MaximumPackagingLength
                        MaximumPackagingWidth
                        NetWeight
                        OvercapacityTolerance
                        PackagingProductGroup
                        PackingReferenceProduct
                        ProdAllocDetnProcedure
                        ProdChmlCmplncRelevanceCode
                        ProdCompetitorCustomerNumber
                        ProdEffctyParamValsAreAssigned
                        ProdIsEnvironmentallyRelevant
                        Product
                        ProductDocumentChangeNumber
                        ProductDocumentNumber
                        ProductDocumentPageCount
                        ProductDocumentPageFormat
                        ProductDocumentPageNumber
                        ProductDocumentType
                        ProductDocumentVersion
                        ProductGroup
                        ProductHierarchy
                        ProductIsConfigurable
                        ProductIsHighlyViscous
                        ProductMeasurementUnit
                        ProductOldID
                        ProductStandardID
                        ProductType
                        ProductVolume
                        ProductionMemoPageFormat
                        ProductionOrInspectionMemoTxt
                        QualityInspectionGroup
                        QuarantinePeriod
                        SerialNoExplicitnessLevel
                        SerialNumberProfile
                        SizeOrDimensionText
                        StandardHandlingUnitType
                        TimeUnitForQuarantinePeriod
                        TransportIsInBulk
                        UnitForMaxPackagingDimensions
                        VolumeUnit
                        WarehouseProductGroup
                        WarehouseStorageCondition
                        WeightUnit
                    )
                    WITH it_product_create
*                   Description
                    CREATE BY \_ProductDescription FIELDS (
*                        Product
                        Language
                        ProductDescription

                    )
                    WITH it_productdescription_create
*                   Description
                    CREATE BY \_ProductEWMWarehouse FIELDS (
*                        Product
                        EWMWarehouse
                        EntitledToDisposeParty
                        ProductInternalUUID
                        SupplyChainUnitUUID
                        EWMPartyEntitledToDisposeUUID
                        BaseUnit
                        EWMProductProcessBlockProfile
                        EWMProcessTypeControlCode
                        EWMProductLoadCategory
                        EWMStggAreaDeterminationGroup
                        EWMIsCnsmpnRlvtForValAddedSrvc
                        RequiredMinShelfLife
                        EWMPtwyControlStrategy
                        EWMStorageSectionMethod
                        EWMStorageBinType
                        EWMBulkStorageMethod
                        EWMStockRemovalControlStrategy
                        EWMStockDeterminationGroup
                        EWMProdTwoStepPickingRelevant
                        EWMSltgRequirementQuantity
                        EWMSltgNumberOfSalesOrderItems
                        EWMSltgRecmddStorageQuantity
                        EWMSlottingWeightSizeCode
                        EWMSlottingVolumeSizeCode
                        EWMSlottingLengthSizeCode
                        EWMSlottingWidthSizeCode
                        EWMSlottingHeightSizeCode
                        EWMPhysInventoryCountingCycle
                        EWMProdBackflushWthdrwlMethod
                        EWMKitQuantityCorrelation
                        EWMQuantityAdjustmentProfile
                        EWMMrchdsDistrQtyClassfctn
                        EWMPreferredUnit
                        EWMQualityInspectionGroup
                    )
                    WITH it_productewmwarehouse_create
*                   Plant
                    CREATE BY \_ProductPlant FIELDS (
                        BaseUnit
                        ConfigurableProduct
                        DistrCntrDistributionProfile
                        FiscalYearVariant
                        GoodsIssueUnit
                        IsBatchManagementRequired
                        IsMarkedForDeletion
                        IsNegativeStockAllowed
                        OriginalBatchReferenceProduct
                        PeriodType
                        Plant
                        ProductCFOPCategory
                        ProductControlTemperatureUnit
                        ProductFreightGroup
                        ProductIsCriticalPrt
                        ProductIsExciseTaxRelevant
                        ProductLogisticsHandlingGroup
                        ProductMaxControlTemperature
                        ProductMinControlTemperature
                        ProfileCode
                        ProfileValidityStartDate
                        ProfitCenter
                        SerialNumberProfile
                        StockDeterminationGroup
                    )
                    WITH it_productplant_create
*                   Procurement
                    CREATE BY \_ProductProcurement FIELDS (
                        PurchaseOrderQuantityUnit
                        PurchasingAcknProfile
                        VarblPurOrdUnitStatus
                    )
                    WITH it_productprocurement_create
*                   Quality Management
                    CREATE BY \_ProductQualityManagement FIELDS (
                        CatalogProfile
                        QltyMgmtInProcmtIsActive
                    )
                    WITH it_productqualitymanage_create
*                   Sales
                    CREATE BY \_ProductSales FIELDS (
                        AllowedPackagingVolumeQty
                        AllowedPackagingVolumeQtyUnit
                        AllowedPackagingWeightQty
                        AllowedPackagingWeightQtyUnit
                        ExcessWeightToleranceValue
                        MaximumLevelByVolumeInPercent
                        PackagingProductType
                        PackggProductIsClosedPackaging
                        ProdExcessVolumeToleranceValue
                        ProductStackingFactor
                        SalesStatus
                        SalesStatusValidityDate
                        TransportationGroup
                    )
                    WITH it_productsales_create
*                   Sales Delivery
                    CREATE BY \_ProductSalesDelivery FIELDS (
                        AccountDetnProductGroup
                        BaseUnit
                        CashDiscountIsDeductible
                        DeliveryNoteProcMinDelivQty
                        DeliveryQuantity
                        DeliveryQuantityUnit
                        FifthSalesSpecProductGroup
                        FirstSalesSpecProductGroup
                        FourthSalesSpecProductGroup
                        IsMarkedForDeletion
                        ItemCategoryGroup
                        LogisticsStatisticsGroup
                        MinimumOrderQuantity
                        PriceSpecificationProductGroup
                        PricingReferenceProduct
                        ProdIsEntlmntRlvt
                        ProductCommissionGroup
                        ProductDistributionChnl
                        ProductHasAttributeID01
                        ProductHasAttributeID02
                        ProductHasAttributeID03
                        ProductHasAttributeID04
                        ProductHasAttributeID05
                        ProductHasAttributeID06
                        ProductHasAttributeID07
                        ProductHasAttributeID08
                        ProductHasAttributeID09
                        ProductHasAttributeID10
                        ProductHierarchy
                        ProductSalesOrg
                        ProductSalesStatus
                        ProductSalesStatusValidityDate
                        RoundingProfile
                        SalesMeasureUnit
                        SecondSalesSpecProductGroup
                        SupplyingPlant
                        ThirdSalesSpecProductGroup
                        VariableSalesUnitIsNotAllowed
                        VolumeRebateGroup
                    )
                    WITH it_productsalesdelivery_create
*                   Storage
                    CREATE BY \_ProductStorage FIELDS (
                        BaseUnit
                        HazardousProduct
                        LabelForm
                        LabelType
                        MinRemainingShelfLife
                        NmbrOfGROrGISlipsToPrintQty
                        ProdTemperatureConditionCode
                        ProductExpirationDateType
                        ShelfLifeExpirationDatePeriod
                        ShelfLifeExprtnDateRndngRule
                        StorageBinInstruction
                        StorageConditions
                        TotalShelfLife
                        TotalShelfLifeStoragePercent
                    )
                    WITH it_productstorage_create
*                   Unit Of Measure
                    CREATE BY \_ProductUnitOfMeasure FIELDS (
                        AlternativeUnit
                        BaseUnit
                        CapacityUsage
                        GlobalTradeItemNumber
                        GlobalTradeItemNumberCategory
                        GrossWeight
                        LowerLevelPackagingUnit
                        MaximumStackingFactor
                        ProductMeasurementUnit
                        ProductVolume
                        QuantityDenominator
                        QuantityNumerator
                        UnitSpecificProductHeight
                        UnitSpecificProductLength
                        UnitSpecificProductWidth
                        VolumeUnit
                        WeightUnit
                    )
                    WITH it_productunitofmeasure_create
*                   Valuation
                    CREATE BY \_ProductValuation FIELDS (
                        BaseUnit
                        Currency
                        InventoryValuationProcedure
                        IsMarkedForDeletion
                        IsProducedInhouse
                        MovingAveragePrice
                        PriceDeterminationControl
                        ProductOriginType
                        ProductPriceUnitQuantity
                        ProductUsageType
                        ProjectStockValuationClass
                        StandardPrice
                        ValuationArea
                        ValuationCategory
                        ValuationClass
                        ValuationClassSalesOrderStock
                        ValuationType
                    )
                    WITH it_productvaluation_create

                    ENTITY ProductPlant
*                   Product Plant - MRP
                    CREATE BY \_ProductPlantMRP FIELDS (
*                        Plant
*                        Product
                        AssemblyScrapPercent
                        BaseUnit
                        Currency
                        DependentRqmtMRPRelevance
                        DfltStorageLocationExtProcmt
                        FixedLotSizeQuantity
                        IsMarkedForDeletion
                        IsPlannedDeliveryTime
                        LotSizeIndependentCosts
                        LotSizeRoundingQuantity
                        LotSizingProcedure
                        MRPArea
                        MRPGroup
                        MRPPlanningCalendar
                        MRPResponsible
                        MRPSafetyStockMethod
                        MRPType
                        MaximumLotSizeQuantity
                        MaximumStockQuantity
                        MinimumLotSizeQuantity
                        PlanAndOrderDayDetermination
                        PlannedDeliveryDurationInDays
                        PlanningTimeFence
                        ProcurementSubType
                        ProductSafetyTimeMRPRelevance
                        ProductServiceLevelInPercent
                        ProductionInvtryManagedLoc
                        RangeOfCvrgPrflCode
                        ReorderThresholdQuantity
                        RoundingProfile
                        RqmtQtyRcptTaktTmeInWrkgDays
                        SafetyStockQuantity
                        SafetySupplyDurationInDays
                        SafetyTimePeriodProfile
                        StorageCostsPercentageCode
                    )
                    WITH it_mrp_create
*                   Product Plant - Supply Planning
                    CREATE BY \_ProductPlantSupplyPlanning FIELDS (
*                        Plant
*                        Product
                        FixedLotSizeQuantity
                        MaximumLotSizeQuantity
                        MinimumLotSizeQuantity
                        LotSizeRoundingQuantity
                        LotSizingProcedure
                        MRPType
                        MRPResponsible
                        SafetyStockQuantity
                        MinimumSafetyStockQuantity
                        PlanningTimeFence
                        ConsumptionValueCategory
                        MaximumStockQuantity
                        ReorderThresholdQuantity
                        PlannedDeliveryDurationInDays
                        SafetySupplyDurationInDays
                        PlanningStrategyGroup
                        TotalReplenishmentLeadTime
                        ProcurementType
                        ProcurementSubType
                        AssemblyScrapPercent
                        AvailabilityCheckType
                        GoodsReceiptDuration
                        PlanAndOrderDayDetermination
                        RoundingProfile
                        DfltStorageLocationExtProcmt
                        MRPGroup
                        LotSizeIndependentCosts
                        RqmtQtyRcptTaktTmeInWrkgDays
                        MRPPlanningCalendar
                        RangeOfCvrgPrflCode
                        ProductSafetyTimeMRPRelevance
                        SafetyTimePeriodProfile
                        DependentRqmtMRPRelevance
                        ProductServiceLevelInPercent
                        ProdInhProdnDurationInWorkDays
                        MRPAvailabilityType
                        CrossProjectProduct
                        StorageCostsPercentageCode
                        FollowUpProduct
                        RepetitiveManufacturingIsAllwd
                        DependentRequirementsType
                        ProductIsBulkComponent
                        RepetitiveManufacturingProfile
                        BackwardCnsmpnPeriodInWorkDays
                        FwdConsumptionPeriodInWorkDays
                        ProdRqmtsConsumptionMode
                        ProdFcstRequirementsSplitCode
                        EffectiveOutDate
                        SchedulingFloatProfile
                        ComponentScrapInPercent
                        ProductDiscontinuationCode
                        ProductRequirementsGrouping
                        ProductionInvtryManagedLoc
                        ProductComponentBackflushCode
                        ProposedProductSupplyArea
                        MRPSafetyStockMethod
                        JITProdnConfProfile
                        PlannedOrderActionControl
                        Currency
                        BaseUnit
                        MRPProfile
                        ProdnPlngAndControlCalendar
                    )
                    WITH it_supplyplanning_create

                    ENTITY ProductSalesDelivery
*                   Prod Sales Delivery - Sales Tax
                    CREATE BY \_ProdSalesDeliverySalesTax FIELDS (
*                        Product
*                        ProductSalesOrg
*                        ProductDistributionChnl
                        Country
                        ProductSalesTaxCategory
                        ProductTaxClassification
                    )
                    WITH it_salestax_create

                    MAPPED DATA(mapped5)
                    FAILED DATA(failed5)
                    REPORTED DATA(reported5).

*               For using in SAVE_MODIFY to Refresh Items and Sizes Tables
                zbp_i_product_001=>mapped_product_uuid = <entity>-ProductUUID.

            ENDLOOP.

**           Behavior class commit executes implicitly (on Save):
*            COMMIT ENTITIES
*                RESPONSE OF I_ProductTP_2
*                FAILED DATA(failed_commit)
*                REPORTED DATA(reported_commit).

        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft
            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Data not saved.' ) ) TO reported-product.
        ENDIF.

    ENDLOOP.

  ENDMETHOD. " create_products

  METHOD check_products. " Check Products

    " Read transfered instances
    READ ENTITIES OF zi_product_001  IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved

*           Check and Update Product Status on Item
            check_items_internal( <entity>-ProductUUID ).

*           Check and Update Product Status on Sizes
            check_sizes_internal( <entity>-ProductUUID ).

        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

    ENDLOOP.

  ENDMETHOD. " check_products

  METHOD on_create. " on initial create

     " Read transfered instances
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved (on pressing down Create)

*           Do nothing

        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft (on pressing up Create)

*           Generate New Matrix ID
            DATA productid TYPE zi_product_001-ProductID VALUE '0000000000'.
            SELECT MAX( productid ) FROM zi_product_001 INTO (@productid).
            productid  = ( productid + 1 ).

*           Main Data:

            MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product
                UPDATE FIELDS ( ProductID ProductType Model Color MatrixTypeID Country )
                WITH VALUE #( (
                    %tky                    = <entity>-%tky
                    ProductID               = productid
                    ProductType             = 'MAT'
                    Model                   = space
                    Color                   = space
                    MatrixTypeID            = space
                    Country                 = 'DE'
                ) )
                FAILED DATA(ls_failed1)
                MAPPED DATA(ls_mapped1)
                REPORTED DATA(ls_reported1).

*           Variant Management Data:

            MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product
                UPDATE FIELDS ( Hidden00 Hidden01 Hidden02 Hidden03 Hidden04 Hidden05 Hidden06 Hidden07 Hidden08 Hidden09 Hidden10 Hidden11 Hidden12 Hidden13 Hidden14 Hidden15 Hidden16 Hidden17 Hidden18 Hidden19 Hidden20 Hidden21 Hidden22 )
                WITH VALUE #( (
                    %tky     = <entity>-%tky
                    Hidden00 = abap_false
                    Hidden01 = abap_true
                    Hidden02 = abap_true
                    Hidden03 = abap_true
                    Hidden04 = abap_true
                    Hidden05 = abap_true
                    Hidden06 = abap_true
                    Hidden07 = abap_true
                    Hidden08 = abap_true
                    Hidden09 = abap_true
                    Hidden10 = abap_true
                    Hidden11 = abap_true
                    Hidden12 = abap_true
                    Hidden13 = abap_true
                    Hidden14 = abap_true
                    Hidden15 = abap_true
                    Hidden16 = abap_true
                    Hidden17 = abap_true
                    Hidden18 = abap_true
                    Hidden19 = abap_true
                    Hidden20 = abap_true
                    Hidden21 = abap_true
                    Hidden22 = abap_true
                ) )
                FAILED DATA(ls_failed2)
                MAPPED DATA(ls_mapped2)
                REPORTED DATA(ls_reported2).

        ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_model_modify. " on modifying model

   " Read transfered instances
    READ ENTITIES OF zi_product_001  IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*       Select Actual Model
        SELECT SINGLE * FROM zi_model_005 WHERE ( ModelID = @<entity>-model ) INTO @DATA(wa_model).

*       Update Matrix Type
        MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
            ENTITY Product
            UPDATE FIELDS ( MatrixTypeID )
            WITH VALUE #( (
                %tky            = <entity>-%tky
                MatrixTypeID    = wa_model-MatrixTypeID
            ) )
            FAILED DATA(ls_failed)
            MAPPED DATA(ls_mapped)
            REPORTED DATA(ls_reported).

    ENDLOOP.

  ENDMETHOD. " on_model_modify

  METHOD on_scheme_save. " on saving scheme (Model + Color + Matrix Type + Country) after modify

    DATA it_sizehead_create TYPE TABLE FOR CREATE zi_product_001\_SizeHead. " Size Head
    DATA it_size_create     TYPE TABLE FOR CREATE zi_product_001\_Size. " Size

    DATA ls_sizehead1 TYPE zi_sizehead_001.
    DATA ls_sizehead2 TYPE zi_sizehead_001.

    DATA v_model        TYPE string VALUE ''.
    DATA v_color        TYPE string VALUE ''.
    DATA v_matrixtypeid TYPE string VALUE ''.
    DATA v_country      TYPE string VALUE ''.
    DATA v_update       TYPE string VALUE ''.

    DATA tabix TYPE sy-tabix.

   " Read transfered instances
    READ ENTITIES OF zi_product_001  IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

*        APPEND VALUE #( %key = key-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Event On Model.' ) ) TO reported-matrix.

        IF ( <entity>-%is_draft = '00' ). " Saved

            " Read and set Model, Color, MatrixTypeID, Country
            v_model         = <entity>-Model.
            v_color         = <entity>-Color.
            v_matrixtypeid  = <entity>-MatrixTypeID.
            v_country       = <entity>-Country.

*           (Re)Create Size Table according to Matrix Type :

*           Read Actual Matrix
            SELECT SINGLE * FROM zproduct001  WHERE ( productuuid = @<entity>-ProductUUID ) INTO @DATA(wa_product).

*           Read Matrix Draft
            SELECT SINGLE * FROM zproduct001d WHERE ( productuuid = @<entity>-ProductUUID ) INTO @DATA(wa_product_draft).

            wa_product_draft-model           = v_model.
            wa_product_draft-color           = v_color.
            wa_product_draft-matrixtypeid    = v_matrixtypeid.
            wa_product_draft-country         = v_country.

*           Set Matrix Type ID according to Model
            SELECT SINGLE * FROM zc_model_005 WHERE ( ModelID = @wa_product_draft-model ) INTO @DATA(wa_model).
            IF ( sy-subrc = 0 ).
                IF ( wa_product_draft-matrixtypeid <> wa_model-MatrixTypeID ).
                    wa_product_draft-matrixtypeid = wa_model-MatrixTypeID.
                    MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                        ENTITY Product
                        UPDATE FIELDS ( MatrixTypeID )
                        WITH VALUE #( (
                            %key            = <entity>-%key
                            MatrixTypeID    = wa_product_draft-MatrixTypeID
                        ) )
                        FAILED DATA(ls_failed1)
                        MAPPED DATA(ls_mapped1)
                        REPORTED DATA(ls_reported1).
                ENDIF.
            ENDIF.

            IF ( <entity>-Copying = abap_true ). " Copy Color
*               If Only Color Changed
                IF ( ( wa_product-model = wa_product_draft-model ) AND ( wa_product-color <> wa_product_draft-color ) AND ( wa_product-matrixtypeid = wa_product_draft-matrixtypeid ) AND ( wa_product-country = wa_product_draft-country ) ).
                    RETURN.
                ENDIF.
            ELSE. " Default Behavior
*               If No Change
                IF ( ( wa_product-model = wa_product_draft-model ) AND ( wa_product-color = wa_product_draft-color ) AND ( wa_product-matrixtypeid = wa_product_draft-matrixtypeid ) AND ( wa_product-country = wa_product_draft-country ) ).
                    RETURN.
                ENDIF.
            ENDIF.

*           Read Actual Size Table
            READ ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product
                BY \_Size
                ALL FIELDS WITH VALUE #( ( ProductUUID = <entity>-ProductUUID ) )
                RESULT DATA(lt_size)
                FAILED DATA(ls_failed2)
                REPORTED DATA(ls_reported2).

            SORT lt_size STABLE BY SizeID.

*           Delete Actual Size Table
            LOOP AT lt_size INTO DATA(ls_size).
                MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                    ENTITY Size
                    DELETE FROM VALUE #( ( SizeUUID = ls_size-SizeUUID ) )
                    FAILED DATA(ls_failed3)
                    MAPPED DATA(ls_mapped3)
                    REPORTED DATA(ls_reported3).
            ENDLOOP.

*           Choose Size Variant:
            DATA(hidden00) = abap_true.
            DATA(hidden01) = abap_true.
            DATA(hidden02) = abap_true.
            DATA(hidden03) = abap_true.
            DATA(hidden04) = abap_true.
            DATA(hidden05) = abap_true.
            DATA(hidden06) = abap_true.
            DATA(hidden07) = abap_true.
            DATA(hidden08) = abap_true.
            DATA(hidden09) = abap_true.
            DATA(hidden10) = abap_true.
            DATA(hidden11) = abap_true.
            DATA(hidden12) = abap_true.
            DATA(hidden13) = abap_true.
            DATA(hidden14) = abap_true.
            DATA(hidden15) = abap_true.
            DATA(hidden16) = abap_true.
            DATA(hidden17) = abap_true.
            DATA(hidden18) = abap_true.
            DATA(hidden19) = abap_true.
            DATA(hidden20) = abap_true.
            DATA(hidden21) = abap_true.
            DATA(hidden22) = abap_true.

            IF ( v_matrixtypeid = 'SLIP' ).
                IF ( v_country = 'FR' ).
                    hidden01 = abap_false.
                ELSEIF ( v_country = 'US' ).
                    hidden02 = abap_false.
                ELSEIF ( v_country = 'GB' ).
                    hidden03 = abap_false.
                ELSE.
                    hidden04 = abap_false.
                ENDIF.
            ELSEIF ( v_matrixtypeid = 'INT' ).
                IF ( v_country = 'FR' ).
                    hidden05 = abap_false.
                ELSEIF ( v_country = 'US' ).
                    hidden06 = abap_false.
                ELSEIF ( v_country = 'GB' ).
                    hidden07 = abap_false.
                ELSE.
                    hidden08 = abap_false.
                ENDIF.
            ELSEIF ( v_matrixtypeid = 'BH' ).
                IF ( v_country = 'FR' ).
                    hidden09 = abap_false.
                ELSEIF ( v_country = 'US' ).
                    hidden10 = abap_false.
                ELSEIF ( v_country = 'GB' ).
                    hidden11 = abap_false.
                ELSE.
                    hidden12 = abap_false.
                ENDIF.
            ELSEIF ( v_matrixtypeid = 'BIKINI' ).
                IF ( v_country = 'FR' ).
                    hidden13 = abap_false.
                ELSEIF ( v_country = 'US' ).
                    hidden14 = abap_false.
                ELSEIF ( v_country = 'GB' ).
                    hidden15 = abap_false.
                ELSE.
                    hidden16 = abap_false.
                ENDIF.
            ELSEIF ( v_matrixtypeid = 'MIEDER' ).
                IF ( v_country = 'FR' ).
                    hidden17 = abap_false.
                ELSEIF ( v_country = 'US' ).
                    hidden18 = abap_false.
                ELSEIF ( v_country = 'GB' ).
                    hidden19 = abap_false.
                ELSE.
                    hidden20 = abap_false.
                ENDIF.
            ELSEIF ( ( v_model IS NOT INITIAL ) AND ( v_color IS NOT INITIAL ) ).
                hidden21 = abap_false. " OhneGr (Default)
            ELSE.
                hidden22 = abap_false. " Dummy
            ENDIF.

            MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                ENTITY Product
                UPDATE FIELDS ( Hidden00 Hidden01 Hidden02 Hidden03 Hidden04 Hidden05 Hidden06 Hidden07 Hidden08 Hidden09 Hidden10 Hidden11 Hidden12 Hidden13 Hidden14 Hidden15 Hidden16 Hidden17 Hidden18 Hidden19 Hidden20 Hidden21 Hidden22 )
                WITH VALUE #( (
                    %tky     = <entity>-%tky
                    Hidden00 = hidden00
                    Hidden01 = hidden01
                    Hidden02 = hidden02
                    Hidden03 = hidden03
                    Hidden04 = hidden04
                    Hidden05 = hidden05
                    Hidden06 = hidden06
                    Hidden07 = hidden07
                    Hidden08 = hidden08
                    Hidden09 = hidden09
                    Hidden10 = hidden10
                    Hidden11 = hidden11
                    Hidden12 = hidden12
                    Hidden13 = hidden13
                    Hidden14 = hidden14
                    Hidden15 = hidden15
                    Hidden16 = hidden16
                    Hidden17 = hidden17
                    Hidden18 = hidden18
                    Hidden19 = hidden19
                    Hidden20 = hidden20
                    Hidden21 = hidden21
                    Hidden22 = hidden22
                ) )
                FAILED DATA(ls_failed4)
                MAPPED DATA(ls_mapped4)
                REPORTED DATA(ls_reported4).

*           Set Criticality01 according to Color Value

            DATA(criticality01) = '0'. " Grey

            CASE v_color.
                WHEN '047'.
                    criticality01 = '1'. " Red
                WHEN '048'.
                    criticality01 = '2'. " Yellow (Orange)
                WHEN '049'.
                    criticality01 = '3'. " Green
                WHEN '050'.
                    criticality01 = '5'. " Blue
            ENDCASE.

*            MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
*                ENTITY Product
*                UPDATE FIELDS ( Criticality01 )
*                WITH VALUE #( (
*                    %key     = <entity>-%key
*                    Criticality01 = criticality01
*                ) )
*                FAILED DATA(ls_failed5)
*                MAPPED DATA(ls_mapped5)
*                REPORTED DATA(ls_reported5).

*           Populate Size table according to Matrix Type and Country:

*           Read Matrix Type
            SELECT SINGLE * FROM zi_matrixtype_005 WHERE ( matrixtypeid = @wa_product_draft-matrixtypeid ) INTO @DATA(wa_matrixtype).

*           Read Matrix Type Table
            READ ENTITIES OF zi_matrixtype_005 " IN LOCAL MODE
                ENTITY MatrixType
                BY \_CupSize
                ALL FIELDS WITH VALUE #( ( MatrixTypeUUID = wa_matrixtype-matrixtypeuuid ) )
                RESULT DATA(lt_cupsize)
                BY \_BackSize
                ALL FIELDS WITH VALUE #( ( MatrixTypeUUID = wa_matrixtype-matrixtypeuuid ) )
                RESULT DATA(lt_backsize)
                FAILED DATA(ls_failed6)
                REPORTED DATA(ls_reported6).

            SORT lt_cupsize STABLE BY Sort CupSizeID.
            SORT lt_backsize STABLE BY Sort BackSizeID.

*           OhneGr (Default)
            IF ( lt_backsize[] IS INITIAL ).
                ls_sizehead1-a = '001'.
                ls_sizehead2-a = '001'.
            ENDIF.

            LOOP AT lt_backsize INTO DATA(ls_backsize).
                tabix = sy-tabix.
                DATA(backSizeXX)    = ls_backsize-%data-BackSizeID.
                CASE wa_product_draft-country.
                    WHEN 'FR'.
                        backSizeXX = ls_backsize-%data-BackSizeFR.
                    WHEN 'US'.
                        backSizeXX = ls_backsize-%data-BackSizeUS.
                    WHEN 'GB'.
                        backSizeXX = ls_backsize-%data-BackSizeGB.
                ENDCASE.
                DATA(backSizeID)    = ls_backsize-%data-BackSizeID.
                CASE tabix.
                    WHEN 1.
                        ls_sizehead1-a = backSizeXX.
                        ls_sizehead2-a = backSizeID.
                    WHEN 2.
                        ls_sizehead1-b = backSizeXX.
                        ls_sizehead2-b = backSizeID.
                    WHEN 3.
                        ls_sizehead1-c = backSizeXX.
                        ls_sizehead2-c = backSizeID.
                    WHEN 4.
                        ls_sizehead1-d = backSizeXX.
                        ls_sizehead2-d = backSizeID.
                    WHEN 5.
                        ls_sizehead1-e = backSizeXX.
                        ls_sizehead2-e = backSizeID.
                    WHEN 6.
                        ls_sizehead1-f = backSizeXX.
                        ls_sizehead2-f = backSizeID.
                    WHEN 7.
                        ls_sizehead1-g = backSizeXX.
                        ls_sizehead2-g = backSizeID.
                    WHEN 8.
                        ls_sizehead1-h = backSizeXX.
                        ls_sizehead2-h = backSizeID.
                    WHEN 9.
                        ls_sizehead1-i = backSizeXX.
                        ls_sizehead2-i = backSizeID.
                    WHEN 10.
                        ls_sizehead1-j = backSizeXX.
                        ls_sizehead2-j = backSizeID.
                    WHEN 11.
                        ls_sizehead1-k = backSizeXX.
                        ls_sizehead2-k = backSizeID.
                    WHEN 12.
                        ls_sizehead1-l = backSizeXX.
                        ls_sizehead2-l = backSizeID.
                ENDCASE.
            ENDLOOP.

            APPEND VALUE #( ProductUUID = <entity>-ProductUUID
                %target = VALUE #( (
                    ProductUUID = wa_product_draft-ProductUUID
                    SizeHeadID  = 1
                    Back        = 'Back (label)'
                    a           = ls_sizehead1-a
                    b           = ls_sizehead1-b
                    c           = ls_sizehead1-c
                    d           = ls_sizehead1-d
                    e           = ls_sizehead1-e
                    f           = ls_sizehead1-f
                    g           = ls_sizehead1-g
                    h           = ls_sizehead1-h
                    i           = ls_sizehead1-i
                    j           = ls_sizehead1-j
                    k           = ls_sizehead1-k
                    l           = ls_sizehead1-l
                ) )
            ) TO it_sizehead_create.

            APPEND VALUE #( ProductUUID = <entity>-ProductUUID
                %target = VALUE #( (
                    ProductUUID = wa_product_draft-ProductUUID
                    SizeHeadID  = 2
                    Back        = 'Back (Id)'
                    a           = ls_sizehead2-a
                    b           = ls_sizehead2-b
                    c           = ls_sizehead2-c
                    d           = ls_sizehead2-d
                    e           = ls_sizehead2-e
                    f           = ls_sizehead2-f
                    g           = ls_sizehead2-g
                    h           = ls_sizehead2-h
                    i           = ls_sizehead2-i
                    j           = ls_sizehead2-j
                    k           = ls_sizehead2-k
                    l           = ls_sizehead2-l
                ) )
            ) TO it_sizehead_create.

*           Delete Obsolete Size Head Table
            SELECT SINGLE * FROM zi_sizehead_001 WHERE ( ( ProductUUID = @<entity>-ProductUUID ) AND ( SizeHeadID = '1' ) ) INTO @DATA(wa_sizehead1).
            IF ( sy-subrc = 0 ).
                MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                    ENTITY SizeHead
                    DELETE FROM VALUE #( ( SizeHeadUUID = wa_sizehead1-SizeHeadUUID ) )
                    FAILED DATA(ls_failed7)
                    MAPPED DATA(ls_mapped7)
                    REPORTED DATA(ls_reported7).
            ENDIF.

            SELECT SINGLE * FROM zi_sizehead_001 WHERE ( ( ProductUUID = @<entity>-ProductUUID ) AND ( SizeHeadID = '2' ) ) INTO @DATA(wa_sizehead2).
            IF ( sy-subrc = 0 ).
                MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
                    ENTITY SizeHead
                    DELETE FROM VALUE #( ( SizeHeadUUID = wa_sizehead2-SizeHeadUUID ) )
                    FAILED DATA(ls_failed8)
                    MAPPED DATA(ls_mapped8)
                    REPORTED DATA(ls_reported8).
            ENDIF.

            " (Re)Create Actual Size Head Table
            MODIFY ENTITY IN LOCAL MODE zi_product_001
                CREATE BY \_SizeHead AUTO FILL CID
                FIELDS ( ProductUUID SizeHeadID Back a b c d e f g h i j k l BackSizeID )
                WITH it_sizehead_create
                FAILED DATA(ls_failed9)
                MAPPED DATA(ls_mapped9)
                REPORTED DATA(ls_reported9).

            IF ( ( v_model IS NOT INITIAL ) AND ( v_color IS NOT INITIAL ) ).
                IF ( lt_cupsize[] IS INITIAL ).
                    APPEND VALUE #( CupSizeID = '0' ) TO lt_cupsize.
                ENDIF.
                LOOP AT lt_cupsize INTO DATA(ls_cupsize).
                    tabix = sy-tabix.
                    APPEND VALUE #(
                        ProductUUID = <entity>-ProductUUID
                        %target = VALUE #( (
                            ProductUUID = wa_product_draft-ProductUUID
                            SizeID      = tabix
                            Back        = ls_cupsize-CupSizeID
                            BackSizeID  = ls_cupsize-CupSizeID
                         ) )
                    ) TO it_size_create.
                ENDLOOP.
            ENDIF.

*           Check and Set Product status on Size Table:

            DATA v_cupsize  TYPE string.
            DATA v_backsize TYPE string.
            DATA v_product  TYPE string.
            SPLIT 'a:01 b:02 c:03 d:04 e:05 f:06 g:07 h:08 i:09 j:10 k:11 l:12' AT space INTO TABLE DATA(columns).
            LOOP AT it_size_create ASSIGNING FIELD-SYMBOL(<wa_size_create>). " Cup
                LOOP AT columns INTO DATA(column). " Back
                    LOOP AT <wa_size_create>-%target ASSIGNING FIELD-SYMBOL(<target>). " target
                        SPLIT column AT ':' INTO DATA(s1) DATA(s2).
                        s2 = 'Criticality' && s2.
                        v_cupsize  = <target>-BackSizeID.
                        v_backsize = ls_sizehead2-(s1). " ls_sizehead2-a
                        CONCATENATE v_model v_color v_cupsize v_backsize INTO v_product SEPARATED BY '-'.

                        DATA(exists) = check_product_internal( v_product ). " raises error

**                       Try Read Product via Entity (it works)
*                        DATA(exists) = abap_false.
*                        READ ENTITIES OF I_ProductTP_2 " IN LOCAL MODE
*                            ENTITY Product
*                            FIELDS ( Product )
*                            WITH VALUE #( ( %key-Product = v_product ) )
*                            RESULT DATA(lt_product)
*                            FAILED DATA(ls_failed10)
*                            REPORTED DATA(ls_reported10).
*                        IF ( lt_product[] IS NOT INITIAL ).
*                            exists = abap_true.
*                        ENDIF.

                        IF ( exists = abap_true ).
                            <target>-(s2) = '1'. " Red  " <target>-Criticality01
                        ELSE.
                            <target>-(s2) = '0'. " None
                        ENDIF.
                    ENDLOOP. " <target>
                ENDLOOP. "Back
            ENDLOOP. " Cup

*           Restore Size Table values (true/false) from Item Table :

*           Read Item Table Draft
            SELECT * FROM zitem001d WHERE ( productuuid = @<entity>-ProductUUID ) ORDER BY itemid INTO TABLE @DATA(it_item_draft) .

            LOOP AT it_item_draft INTO DATA(wa_item_draft) WHERE ( draftentityoperationcode <> 'D' ).
                DATA(product) = CONV string( wa_item_draft-product ).
                SPLIT product AT '-' INTO DATA(model) DATA(color) DATA(cupsize) DATA(backsize).
                IF ( ( model = wa_product_draft-model ) AND ( color = wa_product_draft-color ) ).
                    DATA(value) = abap_true.
                    LOOP AT it_size_create INTO DATA(wa_size_create).
                        DATA(tabix1) = sy-tabix.
                        LOOP AT wa_size_create-%target INTO DATA(target).
                            DATA(tabix2) = sy-tabix.
                            IF ( cupsize = target-Back ).
                                CASE backsize.
                                    WHEN ls_sizehead2-a.
                                        target-a    = value.
                                    WHEN ls_sizehead2-b.
                                        target-b    = value.
                                    WHEN ls_sizehead2-c.
                                        target-c    = value.
                                    WHEN ls_sizehead2-d.
                                        target-d    = value.
                                    WHEN ls_sizehead2-e.
                                        target-e    = value.
                                    WHEN ls_sizehead2-f.
                                        target-f    = value.
                                    WHEN ls_sizehead2-g.
                                        target-g    = value.
                                    WHEN ls_sizehead2-h.
                                        target-h    = value.
                                    WHEN ls_sizehead2-i.
                                        target-i    = value.
                                    WHEN ls_sizehead2-j.
                                        target-j    = value.
                                    WHEN ls_sizehead2-k.
                                        target-k    = value.
                                    WHEN ls_sizehead2-l.
                                        target-l    = value.
                                ENDCASE.
                            ENDIF.
                            MODIFY wa_size_create-%target FROM target INDEX tabix2.
                        ENDLOOP.
                        MODIFY it_size_create FROM wa_size_create INDEX tabix1.
                    ENDLOOP.
                ENDIF.
            ENDLOOP.

            " (Re)Create Actual Size Table
            MODIFY ENTITY IN LOCAL MODE zi_product_001
              CREATE BY \_Size AUTO FILL CID
              FIELDS ( ProductUUID SizeID Back a b c d e f g h i j k l BackSizeID Criticality01 Criticality02 Criticality03 Criticality04 Criticality05 Criticality06 Criticality07 Criticality08 Criticality09 Criticality10 Criticality11 Criticality12 )
              WITH it_size_create
              FAILED DATA(it_failed10)
              MAPPED DATA(it_mapped10)
              REPORTED DATA(it_reported10).

*           Check and Set Status in Sizes
*            check_sizes_internal( <entity>-ProductUUID ). " raises an error

*           Populate Size table according to Matrix Type

        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft
*           Do nothing
        ENDIF.

    ENDLOOP.

  ENDMETHOD. " on_scheme_save

* Convert Sizes to Items
  METHOD sizes_to_items.
*      IMPORTING
*        value(is_draft)             TYPE abp_behv_flag
*        value(i_productuuid)        TYPE zi_matrix_005-ProductUUID
*        value(i_model)              TYPE zi_matrix_005-Model
*        value(i_color)              TYPE zi_matrix_005-Color.

    DATA it_item_create TYPE TABLE FOR CREATE zi_product_001\_Item. " Item
    DATA wa_item_create LIKE LINE OF it_item_create.
    DATA it_item_update TYPE TABLE FOR UPDATE zi_product_001\\Item. " Item
    DATA wa_item_update LIKE LINE OF it_item_update.

    DATA cid TYPE string.

    DATA plant              TYPE string. " VALUE '1000'.
    DATA model              TYPE string.
    DATA color              TYPE string.
    DATA cupsize            TYPE string.
    DATA backsize           TYPE string.
    DATA product            TYPE string.
    DATA quantity           TYPE string.
    DATA stock              TYPE string.
    DATA available_stock    TYPE string.
    DATA availability       TYPE string.
    DATA criticality        TYPE string.
    DATA productURL         TYPE string.

*   Read Actual Matrix
    SELECT SINGLE * FROM zproduct001  WHERE ( productuuid = @i_productuuid ) INTO @DATA(wa_product).

*   Read Matrix Draft
    SELECT SINGLE * FROM zproduct001d WHERE ( productuuid = @i_productuuid ) INTO @DATA(wa_product_draft).

*   Read Size Table (it always reads changed data)
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Size
        ALL FIELDS WITH VALUE #( ( %is_draft = is_draft ProductUUID = i_productuuid ) )
        RESULT DATA(lt_size)
        FAILED DATA(ls_failed1)
        REPORTED DATA(ls_reported1).

    SORT lt_size STABLE BY SizeID.

*   Find max item id
    SELECT MAX( ItemID ) FROM zitem001  WHERE ( ProductUUID = @i_productuuid ) INTO @DATA(maxid).
    SELECT MAX( ItemID ) FROM zitem001d WHERE ( ProductUUID = @i_productuuid ) INTO @DATA(maxid_draft).
    IF ( maxid < maxid_draft ).
        maxid = maxid_draft.
    ENDIF.

*    model       = wa_matrix-Model.
*    color       = wa_matrix-Color.
    model       = i_model.
    color       = i_color.

*   Delete Items with the same Model and Color (in Draft)

*   Read Item Table
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Item
        ALL FIELDS WITH VALUE #( ( %is_draft = is_draft ProductUUID = i_productuuid ) )
        RESULT DATA(lt_item)
        FAILED DATA(ls_read_failed)
        REPORTED DATA(ls_read_reported).

    SORT lt_item STABLE BY ItemID.

*   Delete (Old) Items with the same Model and Color
    LOOP AT lt_item INTO DATA(ls_item) WHERE ( ( Model = i_model ) AND ( Color = i_color ) ).
        MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
            ENTITY Item
            DELETE FROM VALUE #( ( %is_draft = is_draft ItemUUID = ls_item-ItemUUID ) )
            FAILED DATA(ls_delete_failed)
            MAPPED DATA(ls_delete_mapped)
            REPORTED DATA(ls_delete_reported).
    ENDLOOP.

*   Read Actual Size Head Table
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Sizehead
        ALL FIELDS WITH VALUE #( ( ProductUUID = i_productuuid ) )
        RESULT DATA(lt_sizehead)
        FAILED DATA(ls_read_sizehead_failed)
        REPORTED DATA(ls_read_sizehead_reported).

    SORT lt_sizehead STABLE BY SizeHeadID.

    READ TABLE lt_sizehead INTO DATA(ls_sizehead1) WITH KEY SizeHeadID = 1.
    READ TABLE lt_sizehead INTO DATA(ls_sizehead2) WITH KEY SizeHeadID = 2.

*   Add New Items based on Actual Size table
    LOOP AT lt_size INTO DATA(wa_size).
        IF ( ( wa_size-a IS NOT INITIAL ) AND ( ls_sizehead2-a IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-a.
            cupsize     = wa_size-backsizeid.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
            quantity    = wa_size-a.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-b IS NOT INITIAL ) AND ( ls_sizehead2-b IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-b.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-b.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-c IS NOT INITIAL ) AND ( ls_sizehead2-c IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-c.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-c.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-d IS NOT INITIAL ) AND ( ls_sizehead2-d IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-d.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-d.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-e IS NOT INITIAL ) AND ( ls_sizehead2-e IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-e.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-e.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-f IS NOT INITIAL ) AND ( ls_sizehead2-f IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-f.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-f.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-g IS NOT INITIAL ) AND ( ls_sizehead2-g IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-g.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-g.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-h IS NOT INITIAL ) AND ( ls_sizehead2-h IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-h.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-h.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-i IS NOT INITIAL ) AND ( ls_sizehead2-i IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-i.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-i.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-j IS NOT INITIAL ) AND ( ls_sizehead2-j IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-j.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-j.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-k IS NOT INITIAL ) AND ( ls_sizehead2-k IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-k.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-k.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.
        IF ( ( wa_size-l IS NOT INITIAL ) AND ( ls_sizehead2-l IS NOT INITIAL ) ).
            maxid = maxid + 1.
            cid = maxid.
            backsize    = ls_sizehead2-l.
            cupsize     = wa_size-backsizeid.
            quantity    = wa_size-l.
            product     = model && '-' && color && '-' && cupsize && '-' && backsize.
*            productURL  = '/ui#Material-displayFactSheet&/C_ProductObjPg(''' && product && ''')'. " '0205286-705-H-075'
            productURL  = '/ui#Material-manage&/C_Product(Product=''' && product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
            wa_item_create = VALUE #(
                %is_draft  = is_draft
                ProductUUID = i_productuuid
                %target = VALUE #( (
                    %is_draft       = is_draft
                    %cid            = cid
                    ItemID          = cid
                    ProductUUID     = i_productuuid
                    Cupsize         = cupsize
                    Backsize        = backsize
                    Model           = model
                    Color           = color
                    Product         = product
                    Criticality01   = criticality
                    ProductURL      = productURL
                ) )
            ).
            APPEND wa_item_create TO it_item_create.
        ENDIF.

    ENDLOOP.

    " Create New Items
    MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        CREATE BY \_Item
        FIELDS ( ProductUUID ItemID Model Color Backsize Cupsize Product Criticality01 ProductURL )
        WITH it_item_create
        FAILED DATA(ls_failed2)
        MAPPED DATA(ls_mapped2)
        REPORTED DATA(ls_reported2).

*   Renumbering Item Table :

*   Read Item Table
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Item
        ALL FIELDS WITH VALUE #( (
            %is_draft   = is_draft
            ProductUUID  = i_productuuid
        ) )
        RESULT DATA(lt_item2)
        FAILED DATA(ls_read_failed2)
        REPORTED DATA(ls_read_reported2).

*    SORT By Product and Quantity
    SORT lt_item2 STABLE BY Product.

    LOOP AT lt_item2 INTO DATA(ls_item2).
        MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
            ENTITY Item
            UPDATE FIELDS (
                ItemID
            )
            WITH VALUE #( (
                %is_draft   = is_draft
                %key        = ls_item2-%key
                ItemID     = sy-tabix
            ) )
            MAPPED DATA(ls_mapped3)
            FAILED DATA(ls_failed3)
            REPORTED DATA(ls_reported3).

    ENDLOOP.

  ENDMETHOD. " sizes_to_items

  METHOD check_items_internal.

    DATA it_item_update TYPE TABLE FOR UPDATE zi_product_001\\Item. " Item

    DATA status         TYPE zi_item_001-Status.
    DATA criticality01  TYPE zi_item_001-Criticality01.

*   Read Actual Items
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Item
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_item)
        FAILED DATA(ls_failed1)
        REPORTED DATA(ls_reported1).

    LOOP AT lt_item INTO DATA(ls_item).

        DATA(product) = CONV string( ls_item-Product ).

*       Check Existence Status
        DATA(exists) = check_product_internal( product ).
        IF ( exists = abap_true ).
            status          = 'Exists'.
            criticality01   = '1'. " Red
        ELSE.
            status          = space.
            criticality01   = '0'. " None
        ENDIF.

        APPEND VALUE #(
*            %tky            = ls_item-%tky
            itemuuid        = ls_item-ItemUUID
            status          = status
            criticality01   = criticality01
        ) TO it_item_update.

    ENDLOOP.

    MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Item
        UPDATE FIELDS ( Status Criticality01 )
        WITH it_item_update
        FAILED DATA(ls_failed2)
        MAPPED DATA(ls_mapped2)
        REPORTED DATA(ls_reported2).

  ENDMETHOD. " check_products_internal

  METHOD check_product_internal. " Check the Material (exists or does not)

        exists = abap_false.
        IF ( i_product IS NOT INITIAL ).
            READ ENTITIES OF I_ProductTP_2
                ENTITY Product
                FIELDS ( Product IsMarkedForDeletion )
                WITH VALUE #( ( %key-Product = i_product ) )
                RESULT DATA(lt_product)
                FAILED DATA(ls_failed)
                REPORTED DATA(ls_reported).
            LOOP AT lt_product INTO DATA(ls_product) WHERE ( IsMarkedForDeletion = abap_false ).
                exists = abap_true.
            ENDLOOP.
        ENDIF.

  ENDMETHOD. " check_product_internal

  METHOD check_sizes_internal.

    DATA it_size_update TYPE TABLE FOR UPDATE zi_product_001\\Size. " Size

    DATA model          TYPE string.
    DATA color          TYPE string.
    DATA cupsize        TYPE string.
    DATA backsize       TYPE string.
    DATA product        TYPE string.
    DATA criticality    TYPE string.

*   Read Actual Product (node)
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_product)
        FAILED DATA(ls_failed1)
        REPORTED DATA(ls_reported1).

    READ TABLE lt_product INDEX 1 INTO DATA(ls_product).

*   Read Actual SizeHead
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_SizeHead
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_sizehead)
        FAILED DATA(ls_failed2)
        REPORTED DATA(ls_reported2).

    READ TABLE lt_sizehead INTO DATA(ls_sizehead1) WITH KEY SizeHeadID = '1'.
    READ TABLE lt_sizehead INTO DATA(ls_sizehead2) WITH KEY SizeHeadID = '2'.

*   Read Actual Sizes
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Size
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_size)
        FAILED DATA(ls_failed3)
        REPORTED DATA(ls_reported3).

    SORT lt_size STABLE BY SizeID.

    model = ls_product-Model.
    color = ls_product-Color.
    SPLIT 'a:01 b:02 c:03 d:04 e:05 f:06 g:07 h:08 i:09 j:10 k:11 l:12' AT space INTO TABLE DATA(columns).
    LOOP AT lt_size INTO DATA(ls_size). " Cup
        LOOP AT columns INTO DATA(column). " Back
            SPLIT column AT ':' INTO DATA(s1) DATA(s2).
            s2 = 'Criticality' && s2.
            cupsize     = ls_size-BackSizeID.
            backsize    = ls_sizehead2-(s1). " ls_sizehead2-a
            CONCATENATE model color cupsize backsize INTO product SEPARATED BY '-'.
            DATA(exists) = check_product_internal( product ).
            IF ( exists = abap_true ).
                ls_size-(s2) = '1'. " Red " ls_size-Criticality01
            ELSE.
                ls_size-(s2) = '0'. " None
            ENDIF.
        ENDLOOP.
        APPEND VALUE #(
            %key-SizeUUID   = ls_size-SizeUUID
            Criticality01   = ls_size-Criticality01
            Criticality02   = ls_size-Criticality02
            Criticality03   = ls_size-Criticality03
            Criticality04   = ls_size-Criticality04
            Criticality05   = ls_size-Criticality05
            Criticality06   = ls_size-Criticality06
            Criticality07   = ls_size-Criticality07
            Criticality08   = ls_size-Criticality08
            Criticality09   = ls_size-Criticality09
            Criticality10   = ls_size-Criticality10
            Criticality11   = ls_size-Criticality11
            Criticality12   = ls_size-Criticality12
        )
        TO it_size_update.
    ENDLOOP.

    MODIFY ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Size
        UPDATE FIELDS ( Criticality01 Criticality02 Criticality03 Criticality04 Criticality05 Criticality06 Criticality07 Criticality08 Criticality09 Criticality10 Criticality11 Criticality12 )
        WITH it_size_update
        MAPPED DATA(ls_mapped4)
        FAILED DATA(ls_failed4)
        REPORTED DATA(ls_reported4).

  ENDMETHOD. " check_sizes_internal

ENDCLASS. " lhc_Product IMPLEMENTATION


CLASS lsc_zi_product_001 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

  PRIVATE SECTION.

*   Internal methods
    METHODS check_product_internal
      IMPORTING
        VALUE(i_product)     TYPE string
      RETURNING
        VALUE(exists)        TYPE abap_boolean.

    METHODS check_sizes_internal
      IMPORTING
        VALUE(i_productuuid) TYPE zi_product_001-ProductUUID.

    METHODS check_items_internal
      IMPORTING
        VALUE(i_productuuid) TYPE zi_product_001-ProductUUID.

ENDCLASS. " lsc_zi_product_001 DEFINITION

CLASS lsc_zi_product_001 IMPLEMENTATION.

  METHOD save_modified.

        IF ( zbp_i_product_001=>mapped_product_uuid IS NOT INITIAL ).

*           Check and Update Product Status on Item
            check_items_internal( zbp_i_product_001=>mapped_product_uuid ).

*           Check and Update Product Status on Sizes
            check_sizes_internal( zbp_i_product_001=>mapped_product_uuid ).

        ENDIF.

  ENDMETHOD. " save_modified

* Internal Methods:

  METHOD check_product_internal. " Check the Material (exists or does not)

        exists = abap_false.
        IF ( i_product IS NOT INITIAL ).
            READ ENTITIES OF I_ProductTP_2
                ENTITY Product
                FIELDS ( Product )
                WITH VALUE #( ( %key-Product = i_product ) )
                RESULT DATA(lt_product)
                FAILED DATA(ls_failed)
                REPORTED DATA(ls_reported).
            IF ( lt_product[] IS NOT INITIAL ).
                exists = abap_true.
            ENDIF.
        ENDIF.

  ENDMETHOD. " check_product_internal

  METHOD check_items_internal. " Check Materials (exists or doesn't) and Update Value on Sizes

    DATA it_item_update TYPE TABLE FOR UPDATE zi_product_001\\Item. " Item

    DATA status         TYPE zi_item_001-Status.
    DATA criticality01  TYPE zi_item_001-Criticality01.

*   Read Actual Items
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Item
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_item)
        FAILED DATA(ls_failed1)
        REPORTED DATA(ls_reported1).

    LOOP AT lt_item INTO DATA(ls_item).

        DATA(product) = CONV string( ls_item-Product ).

*       Check Existence Status
        DATA(exists) = check_product_internal( product ).
        IF ( exists = abap_true ).
            status          = 'Exists'.
            criticality01   = '1'. " Red
        ELSE.
            status          = space.
            criticality01   = '0'. " None
        ENDIF.

        UPDATE zitem001 SET Status = @status, Criticality01 = @criticality01 WHERE ( itemuuid = @ls_item-ItemUUID ).

    ENDLOOP.

  ENDMETHOD. " check_items_internal

  METHOD check_sizes_internal.

    DATA it_size_update TYPE TABLE FOR UPDATE zi_product_001\\Size. " Size

    DATA model          TYPE string.
    DATA color          TYPE string.
    DATA cupsize        TYPE string.
    DATA backsize       TYPE string.
    DATA product        TYPE string.
    DATA criticality    TYPE string.

*   Read Actual Product (node)
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_product)
        FAILED DATA(ls_failed1)
        REPORTED DATA(ls_reported1).

    READ TABLE lt_product INDEX 1 INTO DATA(ls_product).

*   Read Actual SizeHead
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_SizeHead
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_sizehead)
        FAILED DATA(ls_failed2)
        REPORTED DATA(ls_reported2).

    READ TABLE lt_sizehead INTO DATA(ls_sizehead1) WITH KEY SizeHeadID = '1'.
    READ TABLE lt_sizehead INTO DATA(ls_sizehead2) WITH KEY SizeHeadID = '2'.

*   Read Actual Sizes
    READ ENTITIES OF zi_product_001 IN LOCAL MODE
        ENTITY Product
        BY \_Size
        ALL FIELDS
        WITH VALUE #( (
*            %tky = <entity>-%tky
            ProductUUID = i_productuuid
        ) )
        RESULT DATA(lt_size)
        FAILED DATA(ls_failed3)
        REPORTED DATA(ls_reported3).

    SORT lt_size STABLE BY SizeID.

    model = ls_product-Model.
    color = ls_product-Color.
    SPLIT 'a:01 b:02 c:03 d:04 e:05 f:06 g:07 h:08 i:09 j:10 k:11 l:12' AT space INTO TABLE DATA(columns).
    LOOP AT lt_size INTO DATA(ls_size). " Cup
        LOOP AT columns INTO DATA(column). " Back
            SPLIT column AT ':' INTO DATA(s1) DATA(s2).
            s2 = 'Criticality' && s2.
            cupsize     = ls_size-BackSizeID.
            backsize    = ls_sizehead2-(s1). " ls_sizehead2-a
            CONCATENATE model color cupsize backsize INTO product SEPARATED BY '-'.
            DATA(exists) = check_product_internal( product ).
            IF ( exists = abap_true ).
                ls_size-(s2) = '1'. " Red " ls_size-Criticality01
            ELSE.
                ls_size-(s2) = '0'. " None
            ENDIF.
        ENDLOOP.

        UPDATE zsize001
            SET
                criticality01 = @ls_size-criticality01,
                criticality02 = @ls_size-criticality02,
                criticality03 = @ls_size-criticality03,
                criticality04 = @ls_size-criticality04,
                criticality05 = @ls_size-criticality05,
                criticality06 = @ls_size-criticality06,
                criticality07 = @ls_size-criticality07,
                criticality08 = @ls_size-criticality08,
                criticality09 = @ls_size-criticality09,
                criticality10 = @ls_size-criticality10,
                criticality11 = @ls_size-criticality11,
                criticality12 = @ls_size-criticality12
            WHERE
                ( sizeuuid = @ls_size-SizeUUID ).

    ENDLOOP.

  ENDMETHOD. " check_sizes_internal

ENDCLASS. " lsc_zi_product_001 IMPLEMENTATION
