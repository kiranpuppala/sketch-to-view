# Convert Sketch components to Android Views.

Base project of Sketch to View generator plugin.

## Plugin

![Sketch To View GIF](https://media.giphy.com/media/lodfxE2mi0H15h4hZN/giphy.gif)





## Usage

### Step1: Add the plugin 

1. [Download](Sketch-To-View.sketchplugin.zip) and unzip the plugin. 
2. Open sketch and navigate to Plugins -> Manage Plugins -> Open Plugins Folder 
3. Copy the unzipped plugin to Plugins folder. 
4. Now you should be able to *Sketch To View* option in plugins. 

### Step2: Generating Views

1. Select any group or layer and type ctrl+shift+g and the android view gets 
automatically copied to your clipboard. 

#### Features 
1. LinearLayouts with gradient, stroke, corner radius support. 
2. Cicular shapes with gradient, stroke, corner radius support. 
3. TextView 
4. ImageView

## Development Guide


#### NOTE : Please make sure you have yarn :: [Installing yarn](https://yarnpkg.com/en/docs/install)

* Initial setup

```bash
$ yarn install
```

* Build plugin

```bash
$ yarn build
```

* To watch for changes

```bash
$ yarn watch
```

## Library

* Sketch Javascript API wrapper :: [Purescript Sketch](https://github.com/iarthstar/purescript-sketch)
