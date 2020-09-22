import bpy
import os
from mathutils import Color, Vector

bl_info = {
    "name": "SSGI for Eevee",
    "description": "SSGI addon for Eevee",
    "author": "0451",
    "version": (0, 1, 2),
    "blender": (2, 83, 4),
    "location": "3D View > View",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Material"
}

from bpy.types import (Panel,
                       Menu,
                       Operator,
                       PropertyGroup,
                       )

from bpy.props import (StringProperty,
                       BoolProperty,
                       IntProperty,
                       FloatProperty,
                       FloatVectorProperty,
                       EnumProperty,
                       PointerProperty,
                       )


# ------------------------------------------------------------------------
#    Scene Properties
# ------------------------------------------------------------------------

class SSGI_Properties(PropertyGroup):

    my_bool: BoolProperty(
        name="Enable or Disable",
        description="A bool property",
        default = False
        )

    boost: FloatProperty(
        name = "Boost SSGI",
        description = "A float property",
        default = 0.1,
        min = 0.00,
        max = 1
        )    
    disable: FloatProperty(
        name = "Disable SSGI",
        description = "A float property",
        default = 0,
        min = 0.00,
        max = 1
        )
    roughness: FloatProperty(
        name = "Diffuse GI Roughness",
        description = "A float property",
        default = 0.75,
        min = 0.5,
        max = 1
        )


# ------------------------------------------------------------------------
#    Operators
# ------------------------------------------------------------------------






class One_Click_SetUp(Operator):
    bl_label = "Add SSGI"
    bl_idname = "wm.oneclicksetup"
    

    def execute(self, context):
        #set scene SSR settings
        bpy.context.scene.eevee.use_ssr = True
        bpy.context.scene.eevee.ssr_max_roughness = 1
        bpy.context.scene.eevee.ssr_thickness = 1
        bpy.context.scene.eevee.ssr_border_fade = 0
        bpy.context.scene.eevee.ssr_quality = 0.78
        bpy.context.scene.eevee.use_ssr_halfres = False
        print("SSR settings set to support SSGI materials")
        
        detectedNodegroups = False
        
        for mat in bpy.data.materials:
            if mat.name == '_SSGI_NodeGroups_':
                detectedNodegroups = True
                print("SSGI_NodeGroups found")
                
        if detectedNodegroups == False:
            print("no SSGI_NodeGroups found")
            os.path.abspath(__file__)
            #Material
            filepath = os.path.join(os.path.dirname(os.path.abspath(__file__)), "SSGI_Library.blend\\Material\\")
            material_name = "_SSGI_NodeGroups_"
            bpy.ops.wm.append( filename = material_name, directory = filepath)
            #World
            filepath = os.path.join(os.path.dirname(os.path.abspath(__file__)), "SSGI_Library.blend\\World\\")
            material_name = "_SSGI_WorldNodeGroups_"
            bpy.ops.wm.append( filename = material_name, directory = filepath)
            
            for mat in bpy.data.materials:
                if mat.use_nodes == True:
                    for node in mat.node_tree.nodes:
                        if mat.name == '_SSGI_NodeGroups_':
                            mat.use_fake_user=True
                            
            for mat in bpy.data.worlds:
                if mat.use_nodes == True:
                    for node in mat.node_tree.nodes:
                        if mat.name == '_SSGI_WorldNodeGroups_':
                            mat.use_fake_user=True
                            
            print("appended _SSGI_NodeGroups_ & _SSGI_WorldNodeGroups_ materials")
        

            
        print("finished evaluating SSGI resources")
          
        for mat in bpy.data.materials:
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if node.type in ["BSDF_PRINCIPLED"] and mat.name != '_SSGI_NodeGroups_':
                        
                        #Define OLD NEW
                        old = node
                        new = mat.node_tree.nodes.new("ShaderNodeGroup")
                        #Set group object data
                        new.node_tree = bpy.data.node_groups['_SSGI_Principled_']

                        #SET new attributes from old
                        new.parent = old.parent
                        new.label = old.label
                        new.mute = old.mute
                        new.hide = old.hide
                        new.select = old.select
                        new.location = old.location
                        #cosmetics
                        new.use_custom_color = True
                        new.color = (0.42869, 0.610496, 0.53948)
                        #(0.854993, 0.514918, 0.274677)
                        new.width = 240

                        

                        
                        # inputs SET DEFAULT and LINK
                        for (name, point) in old.inputs.items():
                            input = new.inputs.get(name)
                            if input:
                                input.default_value = point.default_value
                                for link in point.links:
                                    new.id_data.links.new(link.from_socket, input)

                        # outputs SET BSDF Out links
                        for (name, point) in old.outputs.items():
                            output = new.outputs.get(name)
                            if output:
                                for link in point.links:
                                    new.id_data.links.new(output, link.to_socket)
                        
                        #Remove old
                        print("PRINCIPLED BSDF converted to SSGI nodegroup")
                        mat.node_tree.nodes.remove(old)
                        
        print("Finished converting PRINCIPLED BSDF")
        
        for mat in bpy.data.materials:
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if node.type in ["BSDF_DIFFUSE"] and mat.name != '_SSGI_NodeGroups_':
                        
                        #Define OLD NEW
                        old = node
                        new = mat.node_tree.nodes.new("ShaderNodeGroup")
                        #Set group object data
                        new.node_tree = bpy.data.node_groups['_SSGI_Diffuse_']

                        #SET new attributes from old
                        new.parent = old.parent
                        new.label = old.label
                        new.mute = old.mute
                        new.hide = old.hide
                        new.select = old.select
                        new.location = old.location
                        #cosmetics
                        new.use_custom_color = True
                        new.color = (0.42869, 0.610496, 0.53948)
                        new.width = 150
                        
                        # inputs SET DEFAULT and LINK
                        for (name, point) in old.inputs.items():
                            input = new.inputs.get(name)
                            if input:
                                input.default_value = point.default_value
                                for link in point.links:
                                    new.id_data.links.new(link.from_socket, input)

                        # outputs SET BSDF Out links
                        for (name, point) in old.outputs.items():
                            output = new.outputs.get(name)
                            if output:
                                for link in point.links:
                                    new.id_data.links.new(output, link.to_socket)
                                    
                        #Remove old
                        print("DIFFUSE BSDF converted to SSGI nodegroup")
                        mat.node_tree.nodes.remove(old)
                        
        print("Finished converting DIFFUSE BSDF")
        
        #WORLD MATERIAL
        for mat in bpy.data.worlds:
            if mat.use_nodes == True:
                #print(mat)
                for node in mat.node_tree.nodes:
                    #print(node)
                    if node.type in ["OUTPUT_WORLD"] and mat.name != '_SSGI_WorldNodeGroups_': #FINDS World output and adds a note to it. 
                        print("Found output world")
                        
                        worldoutput = node
                        surfacein = worldoutput.inputs[0]
                        
                        
                        if (surfacein.is_linked):
                            print("Surface output is linked, adding world material controller")
                            
                                                
                            controller = mat.node_tree.nodes.new("ShaderNodeGroup")
                            controller.node_tree = bpy.data.node_groups['_SSGI_WorldController_']

                            #SET attributes from World Output Node
                            controller.parent = worldoutput.parent
                            controller.label = worldoutput.label
                            #controller.mute = worldoutput.mute
                            #controller.hide = worldoutputd.hide
                            #controller.select = worldoutput.select
                            controller.location = worldoutput.location
                            controller.location -= Vector((75.0, 0))
                            #cosmetics
                            controller.use_custom_color = True
                            controller.color = (0.42869, 0.610496, 0.53948)
                            controller.width = 50
                            
                            
                            # Set node input
                            for (name, point) in worldoutput.inputs.items():
                                print("W INPUT NAME IS: ", name)
                                input = controller.inputs.get(name)
                                if input:
                                    for link in point.links:
                                        print("W LINK NAME IS: ", link)
                                        controller.id_data.links.new(link.from_socket, input)
                           
                            
                            controller.id_data.links.new(controller.outputs["Output"], worldoutput.inputs[0])
                               
                        else:
                            print("Surface output not linked, world material controller not added")
        
        return {'FINISHED'}

    
class Remove_SSGI_From_Materials(Operator):
    bl_label = "Remove SSGI"
    bl_idname = "wm.remove_ssgi"

    def execute(self, context):
        
        for mat in bpy.data.materials:
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if hasattr(node, 'node_tree') == True:
                        if hasattr(node.node_tree, 'name') == True:
                            if node.type in ["GROUP"] and node.node_tree.name == '_SSGI_Principled_' and mat.name != '_SSGI_NodeGroups_':   
                                #Define OLD NEW
                                old = node
                                new = mat.node_tree.nodes.new('ShaderNodeBsdfPrincipled')

                                #SET new attributes from old
                                new.parent = old.parent
                                new.label = old.label
                                new.mute = old.mute
                                new.hide = old.hide
                                new.select = old.select
                                new.location = old.location
                                
                                # inputs SET DEFAULT and LINK
                                for (name, point) in old.inputs.items():
                                    input = new.inputs.get(name)
                                    if input:
                                        input.default_value = point.default_value
                                        for link in point.links:
                                            new.id_data.links.new(link.from_socket, input)

                                # outputs SET BSDF Out links
                                for (name, point) in old.outputs.items():
                                    output = new.outputs.get(name)
                                    if output:
                                        for link in point.links:
                                            new.id_data.links.new(output, link.to_socket)

                                #Remove old
                                print(node.node_tree.name, "converted to PRINCIPLED BSDF")
                                mat.node_tree.nodes.remove(old)

        for mat in bpy.data.materials:
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if hasattr(node, 'node_tree') == True:
                        if hasattr(node.node_tree, 'name') == True:
                            if node.type in ["GROUP"] and node.node_tree.name == '_SSGI_Diffuse_' and mat.name != '_SSGI_NodeGroups_':
                                
                                #Define OLD NEW
                                old = node
                                new = mat.node_tree.nodes.new('ShaderNodeBsdfDiffuse')

                                #SET new attributes from old
                                new.parent = old.parent
                                new.label = old.label
                                new.mute = old.mute
                                new.hide = old.hide
                                new.select = old.select
                                new.location = old.location
                                
                                # inputs SET DEFAULT and LINK
                                for (name, point) in old.inputs.items():
                                    input = new.inputs.get(name)
                                    if input:
                                        input.default_value = point.default_value
                                        for link in point.links:
                                            new.id_data.links.new(link.from_socket, input)

                                # outputs SET BSDF Out links
                                for (name, point) in old.outputs.items():
                                    output = new.outputs.get(name)
                                    if output:
                                        for link in point.links:
                                            new.id_data.links.new(output, link.to_socket)
                                            
                                #Remove old
                                print(node.node_tree.name, "converted to DIFFUSE BSDF")
                                mat.node_tree.nodes.remove(old)
        
        #Remove World                        
        for mat in bpy.data.worlds:
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if hasattr(node, 'node_tree') == True:
                        if hasattr(node.node_tree, 'name') == True:
                            if node.type in ["GROUP"] and node.node_tree.name == '_SSGI_WorldController_' and mat.name != '_SSGI_WorldNodeGroups_':
                                print("Removing World Output controller:")
                                
                                controller = node
                                #don't do stuff like this:
                                check_in = 0
                                check_out = 0

                                #get node connected to input
                                for con_inputs in controller.inputs:
                                    for node_links in con_inputs.links:
                                        print("World controller input: ", node_links.from_node.name)
                                        controller_in = node_links.from_node
                                        check_in = 1
                                        

                                #get node connected to output
                                for con_outputs in controller.outputs:
                                    for node_links in con_outputs.links:
                                        print("World controller output: ", node_links.to_node.name)
                                        controller_out = node_links.to_node
                                        check_out = 1
                                
                                #link input node to output node
                                #controller_in.id_data.links.new(controller_in.outputs[0], controller_out.inputs[0])
                                if check_in == 1 and check_out == 1:
                                    print("Attempting to link world output back")
                                    mat.node_tree.links.new(controller_in.outputs[0], controller_out.inputs[0])
                                
                                #Remove controller
                                mat.node_tree.nodes.remove(controller)
                        
        for mat in bpy.data.materials: #remove Fake user from lib material
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if mat.name == '_SSGI_NodeGroups_':
                        mat.use_fake_user=False
                        print("remove fake user from SSGI lib")
                        #WIP Remove from blend
                        
        for mat in bpy.data.worlds: #remove Fake user from lib material
            if mat.use_nodes == True:
                for node in mat.node_tree.nodes:
                    if mat.name == '_SSGI_WorldNodeGroups_':
                        mat.use_fake_user=False
                        print("remove fake user from SSGI World lib")
        
        bpy.data.materials.remove(bpy.data.materials["_SSGI_NodeGroups_"])
        
        #deprecated
        for mat in bpy.data.node_groups:
            if mat.name == '_Boost_SSGI_':
                bpy.data.node_groups.remove(bpy.data.node_groups["_Boost_SSGI_"])
        
        bpy.data.node_groups.remove(bpy.data.node_groups["_Alpha_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_BOOST GI Control_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_BOOST GI_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_DIFFUSE GI Roughness_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_DiffuseRoughness Container_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_DISABLE SSGI_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_Fresnel_SSGI_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_Glossy_Dielectrics_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_Metallic SSR/SSGI switch_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI FIX Baking_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI Mix Controls_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_Diffuse_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_Principled_"])
        bpy.data.node_groups.remove(bpy.data.node_groups["_SubtractiveCol_"])
        
        bpy.data.worlds.remove(bpy.data.worlds["_SSGI_WorldNodeGroups_"])
        #new ones # move to loop in future
        for mat in bpy.data.node_groups:
            if mat.name == '_ClampInputs_':
                bpy.data.node_groups.remove(bpy.data.node_groups["_ClampInputs_"])
        for mat in bpy.data.node_groups:
            if mat.name == '_SSGI_Dielectric_':
                bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_Dielectric_"])
        for mat in bpy.data.node_groups:
            if mat.name == '_SSGI_ScatterNormals_':
                bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_ScatterNormals_"])
        for mat in bpy.data.node_groups:
            if mat.name == '_SSGI_WorldController_':
                bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_WorldController_"])
        for mat in bpy.data.node_groups:
            if mat.name == '_SSGI_WCtrl_':
                bpy.data.node_groups.remove(bpy.data.node_groups["_SSGI_WCtrl_"])
        
        
        print("Finished removing SSGI resources")
        return {'FINISHED'}
    
class Irradiance_Bake_Default(Operator):
    bl_label = "Bake Indirect Lighting (Default)"
    bl_idname = "wm.default_bake"

    def execute(self, context):

        bpy.ops.scene.light_cache_bake()
        
        print("Baked Default Irradiance Lighting")
        return {'FINISHED'}
    
class Irradiance_Bake(Operator):
    bl_label = "Bake Indirect Lighting (Alternative)"
    bl_idname = "wm.fixed_bake"

    def execute(self, context):

        bpy.data.node_groups["_SSGI_Principled_"].nodes["_SSGI_Principled_BakeFix_"].inputs[1].default_value = 1
        bpy.ops.scene.light_cache_bake()
        bpy.data.node_groups["_SSGI_Principled_"].nodes["_SSGI_Principled_BakeFix_"].inputs[1].default_value = 0
        
        print("Baked Fixed Irradiance Lighting")
        return {'FINISHED'}
    
class Irradiance_Bake_Delete(Operator):
    bl_label = "Delete Lighting Cache"
    bl_idname = "wm.delete_bake"

    def execute(self, context):

        bpy.ops.scene.light_cache_free()
        
        print("Deleted Lighting Cache")
        return {'FINISHED'}

# ------------------------------------------------------------------------
#    UI
# ------------------------------------------------------------------------

class SSGI_UI(Panel):
    bl_label = "SSGI for Eevee 0.1.2"
    bl_idname = "OBJECT_PT_custom_panel"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'SSGI'

    def draw(self, context):
        layout = self.layout
        scene = context.scene
        ssgi_t = scene.ssgi_tool
        
        #layout.label(text="Enable while Baking Indirect Light:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        #layout.prop(bpy.data.node_groups["_SSGI_Principled_"].nodes["_SSGI_Principled_BakeFix_"].inputs[1], "default_value",text="Fix While Baking")
        layout.operator("wm.default_bake")
        layout.operator("wm.fixed_bake")
        layout.operator("wm.delete_bake")
        #layout.prop(ssgi_t, "my_bool")
        #layout.prop(ssgi_t, "boost")
        #layout.prop(ssgi_t, "disable")
        #layout.prop(ssgi_t, "roughness")
        #layout.separator() wm.oneclicksetup
        layout.label(text="Setup SSGI:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        layout.operator("wm.oneclicksetup")
        layout.operator("wm.remove_ssgi")
       
        #layout.label(text="set by step", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        layout.label(text="SSGI Controls:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        layout.prop(bpy.data.node_groups["_BOOST GI Control_"].nodes["_BOOST_GI_Node_"].inputs[1], "default_value",text="Intensity")
        layout.prop(bpy.data.node_groups["_SSGI_"].nodes["_SSGI_ScatterNormalsNode_"].inputs[2], "default_value",text="Scatter Diffuse Normals")
        layout.prop(bpy.data.node_groups["_SSGI_Principled_"].nodes["_Glossy_Dielectrics_Node_"].inputs[0], "default_value",text="Blend to SSR on Dielectrics")
        
        layout.label(text="Tweaks:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        layout.prop(bpy.data.node_groups["_BOOST GI Control_"].nodes["_ClampInputsNode_"].inputs[3], "default_value",text="Clamp Input Colors")
        layout.prop(bpy.data.node_groups["_DiffuseRoughness Container_"].nodes["_DIFFUSE_SSGI_Rougness_Node_"].inputs[0], "default_value",text="Diffuse Roughness")
        
        #world controller
        layout.label(text="World Material Ray Visibility:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        layout.prop(bpy.data.node_groups["_SSGI_WorldController_"].nodes["_SSGI_WCtrlNode_"].inputs[1], "default_value",text="Camera Strength")
        layout.prop(bpy.data.node_groups["_SSGI_WorldController_"].nodes["_SSGI_WCtrlNode_"].inputs[2], "default_value",text="Diffuse Strength")
        layout.prop(bpy.data.node_groups["_SSGI_WorldController_"].nodes["_SSGI_WCtrlNode_"].inputs[3], "default_value",text="Glossy Strength (broken)")

        
class SSGI_UI_Tweaks(Panel):
    bl_label = "SSGI Settings"
    bl_idname = "OBJECT_PT_custom_panel_"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'SSGI'
    bl_options = {'DEFAULT_CLOSED'}
    
    def draw(self, context):
        layout = self.layout
        scene = context.scene
        ssgi_t = scene.ssgi_tool
        
        layout.label(text="Tweaks:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        
        layout.prop(bpy.data.node_groups["_SSGI_"].nodes["_SubtractiveCol_Node_"].inputs[1], "default_value",text="Remove Unneeded BSDF")
        layout.prop(bpy.data.node_groups["_SSGI_Principled_"].nodes["_Glossy_Dielectrics_Node_"].inputs[1], "default_value",text="Reduce SSR brightness (1 - correct)")
        layout.prop(bpy.data.node_groups["_SSGI_Principled_"].nodes["_Fresnel_SSGI_Node_"].inputs[3], "default_value",text="Add Fresnel to SSGI")
        layout.prop(bpy.data.node_groups["_SSGI Mix Controls_"].nodes["_DISABLE_SSGI_Node_"].inputs[2], "default_value",text="Disable SSGI/SSR")
        layout.label(text="SSR Controls:", text_ctxt="", translate=False, icon='NONE', icon_value=0)
        layout.prop(context.scene.eevee, "ssr_thickness",text="SSR Thickness")
        layout.prop(context.scene.eevee, "ssr_quality",text="SSR Trace Precision")
        


# ------------------------------------------------------------------------
#    Registration
# ------------------------------------------------------------------------

classes = (
    One_Click_SetUp,
    Remove_SSGI_From_Materials,
    Irradiance_Bake_Default,
    Irradiance_Bake,
    Irradiance_Bake_Delete,
    SSGI_Properties,
    SSGI_UI,
    SSGI_UI_Tweaks,
)

def register():
    from bpy.utils import register_class
    for cls in classes:
        register_class(cls)

    bpy.types.Scene.ssgi_tool = PointerProperty(type=SSGI_Properties)

def unregister():
    from bpy.utils import unregister_class
    for cls in reversed(classes):
        unregister_class(cls)
    del bpy.types.Scene.ssgi_tool


if __name__ == "__main__":
    register()
