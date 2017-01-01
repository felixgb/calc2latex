<template>
  <div class="calc">
    <p>
    Enter a lambda calculus expression, like:
    <pre>\x . x</pre>
    And submit to see a derivation proof tree, or an error message.
    </p>
    <form v-on:submit.prevent="getImgData">
      <input type="text" v-model="code">
      <button type="submit">Derivate!</button>
    </form>
    <span class="error" v-show="!dataUrl">
      No expression entered!
    </span>
    <br>
    <img v-bind:src="dataUrl">
  </div>
</template>

<script>
export default {
  name: 'calc',
  data () {
    return {
      code: '',
      dataUrl: ''
    }
  },
  methods: {
    getImgData: function () {
      this.$http.get('http://localhost:3000/' + btoa(this.code)).then((response) => {
        this.dataUrl = 'data:image/png;base64,' + response.body
      }, (response) => {
        console.log('bad')
      })
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.calc {
  display: flex;
  flex-direction: column;
  padding: 5px;
}

p {
  padding-top: 10px;
  padding-bottom: 10px;
}

pre {
  background-color: #eeeeee;
  padding: 10px;
  padding: 10px;
}

.error {
  color: red;
}
</style>
